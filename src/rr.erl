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
    [{input_file,     $i,          "input",   string, 
      "Input data set"},
     {classifiers,       $m,          undefined, {integer, 10},
      "Number of rulesets to generate"},
     {cores,          $c,           undefined, {integer, erlang:system_info(schedulers)},
      "Number of cores to use when evaluating and building the model"},
     {split,          $s,           undefined, {float, 0.66},
      "Spliting ratio Train/Test"}].

main(Args) ->
    rr_example:init(),
    Options = case getopt:parse(cmd_spec(), Args) of
		  {ok, Parsed} -> 
		      Parsed;
		  {error, _} ->
		      illegal()		      
	      end,
    InputFile = get_opt(input_file, fun illegal/0, Options),
    Classifiers = get_opt(classifiers, fun illegal/0, Options),
    Cores = get_opt(cores, fun illegal/0, Options),
    Split = get_opt(split, fun illegal/0, Options),
    
    io:format("Evaluating '~s' using ~p trees on ~p cores \n", [InputFile, Classifiers, Cores]),

    Then = now(),

    Csv = csv:reader(InputFile),
    {Features, Examples} = rr_example:load(Csv, Cores),
    {Train, Test} = rr_example:split_dataset(Examples, Split),
    Conf = #rr_conf{
	      cores = Cores,
	      score = fun rr_tree:random_score/2,
	      prune = rr_tree:example_depth_stop(2, 1000),
	      evaluate = rr_tree:random_evaluator(0.1), %fun best_subset_evaluate_split/4, 
	      base_learner = {Classifiers, rr_tree},
	      max_id = rr_example:count(Examples)},
    Model = rr_ensamble:generate_model(Features, Train, Conf),
    Dict = rr_ensamble:evaluate_model(Model, Test, Conf),

    io:format("Model accuracy: ~p ~n", [rr_eval:accuracy(Dict)]),
    Now = now(),
    io:format(standard_error, "*** Model evaluated in ~p second(s)*** ~n", 
	      [timer:now_diff(Now, Then) / 1000000]).
    
    

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

show_information() -> 
    io_lib:format("Rule learner, Version (of ~s) ~p.~p.~s ~nAll rights reserved ~s", 
		  [?DATE, ?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?AUTHOR]).
