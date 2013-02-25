%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').

-define(DATE, "2013-02-10").
-define(MAJOR_VERSION, 0).
-define(MINOR_VERSION, 1).
-define(REVISION, 'beta-3').

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").

-include("rr_tree.hrl").

cmd_spec() ->
    [{help,           $h,           "help",         undefined,
      "Show this help"},
     {input_file,     $i,           "input",        string, 
      "Input data set"},
     {classifiers,    $m,           undefined,     {integer, 10},
      "Number of rulesets to generate"},
     {cores,          $c,           undefined,     {integer, erlang:system_info(schedulers)},
      "Number of cores to use when evaluating and building the model"},
     {split,          $s,           undefined,     {float, 0.66},
      "Spliting ratio Train/Test"},
     {score,          undefined,    "score",       {atom, info},
      "Scoring function"},
     {eval,           undefined,    "eval",        {atom, log},
      "Feature evaluation strategies (log/resample/mtry)"},
     {max_depth,      undefined,    "depth",       {integer, 1000},
      "Max depth of single decision tree"},
     {min_example,    undefined,    "examples",    {integer, 2},
      "Min number of examples allowed in split"},
     {no_resamples,   undefined,    "resample",    {integer, 6},
      "Resample N random features K times if gain =< min-gain"},
     {min_gain,       undefined,    "min-gain",    {float, 0},
      "If eval=resample, min-gain controls the minimum allowed gain for not resampling"},
     {no_features,    undefined,    "no-features", {integer, 1},
      "If eval=mtry, no-features controls the number of features to inspect at each split"}
    ].

main(Args) ->
    rr_example:init(),
    Options = case getopt:parse(cmd_spec(), Args) of
		  {ok, Parsed} -> 
		      Parsed;
		  {error, _} ->
		      illegal()		      
	      end,
    case has_opt(help, Options) of
	true ->
	    illegal();
	_ ->
	    ok
    end,
    InputFile = get_opt(input_file, Options),
    Classifiers = get_opt(classifiers, Options),
    Cores = get_opt(cores, Options),
    Split = get_opt(split, Options),
    Eval = case get_opt(eval, Options) of
	       log ->
		   fun rr_tree:best_subset_evaluate_split/4;
	       ntry ->
		   _NoFeatures = get_opt(no_features, Options),
		   illegal();
	       resample ->
		   NoResamples = get_opt(no_resamples, Options),
		   MinGain = get_opt(min_gain, Options),
		   rr_tree:resampled_evaluate(NoResamples, MinGain)		   
	    end,
    Score = case get_opt(score, Options) of
		info ->
		    fun rr_tree:info/2;
		gini ->
		    fun rr_tree:gini/2
	    end,
    MaxDepth = get_opt(max_depth, Options),
    MinEx = get_opt(min_example, Options),
    
    io:format(standard_error, "*** Evaluating '~s' using ~p trees on ~p cores *** \n", [InputFile, Classifiers, Cores]),

    random:seed(now()),
    Csv = csv:reader(InputFile),
    {Features, Examples0} = rr_example:load(Csv, Cores),
    Examples = rr_example:suffle_dataset(Examples0),
    {Train, Test} = rr_example:split_dataset(Examples, Split),
    Conf = #rr_conf{
	      cores = Cores,
	      score = Score,
	      prune = rr_tree:example_depth_stop(MinEx, MaxDepth),
	      evaluate = Eval,
	      split = fun rr_tree:random_split/3,
	      base_learner = {Classifiers, rr_tree},
	      no_features = length(Features)},

    Then = now(),
    Model = rr_ensamble:generate_model(ordsets:from_list(Features), Train, Conf),
    Dict = rr_ensamble:evaluate_model(Model, Test, Conf),
    Time = timer:now_diff(now(), Then) / 1000000,

    NoTestExamples = rr_example:count(Test),
    io:format("Accuracy: ~p ~n", [rr_eval:accuracy(Dict)]),

    Auc = rr_eval:auc(Dict, NoTestExamples),
    io:format("Area under ROC~n"),
    lists:foreach(fun({Class, A}) ->
			  io:format(" * ~s: ~p ~n", [Class, A])
		  end, Auc),
    io:format("Average:~p ~n", [lists:foldl(fun({_, P}, A) -> A + P / length(Auc) end, 0, Auc)]),
    
    io:format("Precision~n"),
    lists:foreach(fun({Class, P}) ->
			  io:format(" * ~s: ~p ~n", [Class, P])
		  end, rr_eval:precision(Dict)),

    Brier = rr_eval:brier(Dict, NoTestExamples),
    io:format("Brier: ~p ~n", [Brier]),
    io:format("Time: ~p seconds ~n", [Time]),
    io:format(standard_error, "*** Model evaluated in ~p second(s)*** ~n", [Time]).    
    

%%
%% Halts the program if illegal arguments are supplied
%%
illegal() ->
    getopt:usage(cmd_spec(), "rr"),
    halt().

%%
%% Get command line option Arg, calling Fun1 if not found
%%	     
get_opt(Arg, Fun1, {Options, _}) ->	
    case lists:keyfind(Arg, 1, Options) of
	{Arg, Ws} ->
	    Ws;
	false -> 
	    Fun1()
    end.

get_opt(Arg, Options) ->
    get_opt(Arg, fun illegal/0, Options).

%%
%% Return true if Arg exist
%%
has_opt(Arg, {Options, _ }) ->
    lists:any(fun (K) ->
		      K == Arg
	      end, Options).
    

show_information() -> 
    io_lib:format("Rule learner, Version (of ~s) ~p.~p.~s ~nAll rights reserved ~s", 
		  [?DATE, ?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?AUTHOR]).
