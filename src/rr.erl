%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').

-define(DATE, "2013-02-26").
-define(MAJOR_VERSION, "0").
-define(MINOR_VERSION, "1").
-define(REVISION, "0.1").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").

-include("rr_tree.hrl").

-define(CMD_SPEC,
	[{help,           $h,           "help",         undefined,
	  "Show this help"},
	 {version,        $v,           "version",      undefined,
	  "Show the version"},
	 {input_file,     $i,           "input",        string, 
	  "Input data set"},
	 {cores,          $c,           undefined,     {integer, erlang:system_info(schedulers)},
	  "Number of cores to use when evaluating and building the model"},
	 {split,          $s,           undefined,     {float, 0.66},
	  "Spliting ratio Train/Test"},
	 {score,          undefined,    "score",       {atom, info},
	  "Measure for evaluating the goodness of a split"},

	 {classifiers,    $m,           "no-trees",     {integer, 10},
	  "Number of trees to generate"},

	 {max_depth,      undefined,    "max-depth",       {integer, 1000},
	  "Max depth of single decision tree"},
	 {min_example,    undefined,    "min-examples",    {integer, 2},
	  "Min number of examples allowed in split"},

	 {weka,           undefined,    "weka",        undefined,
	  "Same as --resample, however with K=inf"},
	 {resample,       undefined,    "resample",    undefined,
	  "Resample N random features K times if gain =< min-gain"},

	 {no_resamples,   undefined,    "no-resample", {integer, 6},
	  "Number of times to resample, if best gain =< --min-gain"},
	 {min_gain,       undefined,    "min-gain",    {float, 0},
	  "Controls the minimum allowed gain for not resampling"},
	 {no_features,    undefined,    "no-features", {integer, -1},
	  "Control the number of features to inspect at each split"}
	]).

main(Args) ->
    rr_example:init(),
    Options = case getopt:parse(?CMD_SPEC, Args) of
		  {ok, Parsed} -> 
		      Parsed;
		  {error, _} ->
		      illegal()		      
	      end,
    case any_opt([help, version], Options) of
	help ->
	    illegal();
	version ->
	    io:format(show_information()),
	    halt();
	false ->
	    ok
    end,
    InputFile = get_opt(input_file, Options),
    Split = get_opt(split, Options),
    Cores = get_opt(cores, Options),
        
    io:format(standard_error, "*** Loading '~s' on ~p core(s) *** \n", [InputFile, Cores]),

    random:seed(now()),
    Csv = csv:reader(InputFile),
    {Features, Examples0} = rr_example:load(Csv, Cores),
    TotalNoFeatures = length(Features),

    Examples = rr_example:suffle_dataset(Examples0),
    {Train, Test} = rr_example:split_dataset(Examples, Split),

    NoFeatures = case get_opt(no_features, Options) of
		     X when X =< 0 ->
			 round(math:log(TotalNoFeatures)/math:log(2)) + 1;
		     X ->
			 X
		 end,	
    Classifiers = get_opt(classifiers, Options),
    Eval = case any_opt([weka, resample], Options) of
		weka ->
		    rr_tree:weka_evaluate(NoFeatures);
		resample ->
		    NoResamples = get_opt(no_resamples, Options),
		    MinGain = get_opt(min_gain, Options),
		    rr_tree:resampled_evaluate(NoResamples, NoFeatures, MinGain);
		false ->
		   rr_tree:subset_evaluate(NoFeatures)
	    end,

    Score = case get_opt(score, Options) of
		info ->
		    fun rr_tree:info/2;
		gini ->
		    fun rr_tree:gini/2
	    end,
    MaxDepth = get_opt(max_depth, Options),
    MinEx = get_opt(min_example, Options),

    Conf = #rr_conf{
	      cores = Cores,
	      score = Score,
	      prune = rr_tree:example_depth_stop(MinEx, MaxDepth),
	      evaluate = Eval,
	      split = fun rr_tree:random_split/3,
	      base_learner = {Classifiers, rr_tree},
	      no_features = TotalNoFeatures },
    io:format(standard_error, "*** Building model using ~p trees and ~p features *** ~n",
	      [Classifiers, NoFeatures]),

    Then = now(),
    Model = rr_ensamble:generate_model(ordsets:from_list(Features), Train, Conf),
    Dict = rr_ensamble:evaluate_model(Model, Test, Conf),
    Time = timer:now_diff(now(), Then) / 1000000,
    io:format(standard_error, "*** Model build in ~p second(s)*** ~n", [Time]),
    io:format(standard_error, "*** Evaluating model using test set *** ~n~n", []),
    
    NoTestExamples = rr_example:count(Test),
    io:format("*** Start ***~n"),
    io:format("File: ~p ~n", [InputFile]),
    io:format("Trees: ~p ~n", [Classifiers]),
    io:format("Features: ~p ~n", [NoFeatures]),
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
    io:format("*** End ***~n").   

%%
%% Halts the program if illegal arguments are supplied
%%
illegal() ->
    getopt:usage(?CMD_SPEC, "rr"),
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

any_opt([], _) ->
    false;
any_opt([O|Rest], Options) ->
    case has_opt(O, Options) of
	true ->
	    O;
	false ->
	    any_opt(Rest, Options)
    end.


%%
%% Return true if Arg exist
%%
has_opt(Arg, {Options, _ }) ->
    lists:any(fun (K) ->
		      K == Arg
	      end, Options).
    

show_information() -> 
    io_lib:format("rr (Random Rule Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s

Written by ~s", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR, ?AUTHOR]).
