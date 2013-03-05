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
	 
	 {split,          $s,           "split",       undefined,
	  "Split data set according to --ratio"},
	 {ratio,          $r,           "ratio",       {float, 0.66},
	  "Splitting ratio (i.e. ratio is the fraction of training examples)"},
	 {cv,             $x,           "cross-validate", undefined,
	  "Cross validation"},
	 {folds,          undefined,    "folds",       {integer, 10},
	  "Number of cross validation folds"},

	 {progress,       undefined,    "progress",    {atom, dots},
	  "Showing the progress"},

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
	 {sqrt,           undefined,    "sqrt",        undefined,
	  "Use sqrt(|Features|) at each node"},

	 {no_resamples,   undefined,    "no-resample", {integer, 6},
	  "Number of times to resample, if best gain =< --min-gain"},
	 {min_gain,       undefined,    "min-gain",    {float, 0},
	  "Controls the minimum allowed gain for not resampling"},
	 {no_features,    undefined,    "no-features", {integer, -1},
	  "Control the number of features to inspect at each split"},
	 {log,            $l,           "log"  ,       {atom, none},
	  "Log level (info, debug, error, none)"},
	 {log_file,     undefined,      "log-target",  undefined,
	  "Debug output source"}
	]).

main(Args) ->
    rr_example:init(),
    random:seed(now()),

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
    Logger = case get_opt(log_file, fun () ->
					    Log = rr_log:new(std_err, get_opt(log, Options)),
					    fun (Level, Message, Params) ->
						    rr_log:log(Log, Level, Message, Params)
					    end
				    end, Options) of
		 _ ->
		     ok
	     end,
    Logger(info, "Starting application..", []),
    


    InputFile = get_opt(input_file, Options),
    Cores = get_opt(cores, Options),
    RunExperiment = case any_opt([cv, split], Options) of
			split ->
			    fun run_split/4;
			cv ->
			    fun run_cross_validation/4;
			false ->
			    io:format(standard_error, "Must select --split or --cross-validation \n", []),
			    illegal()
		    end,
    Progress = case get_opt(progress, Options) of
		   dots ->
		       fun(_, _) -> io:format(standard_error, "..", []) end;
		   numeric ->
		       fun(Id, T) -> io:format(standard_error, "~p/~p.. ", [Id, T]) end;
		   none ->
		       fun(_, _) -> ok end;
		   _ ->
		       illegal()			   
	       end,
    
    io:format(standard_error, "*** Loading '~s' on ~p core(s) *** \n", [InputFile, Cores]),
    Csv = csv:reader(InputFile),
    {Features, Examples0} = rr_example:load(Csv, Cores),
    TotalNoFeatures = length(Features),
    Examples = rr_example:suffle_dataset(Examples0),


    NoFeatures = case any_opt([sqrt, no_features], Options) of
		     false ->
			 case get_opt(no_features, Options) of
			     X when X =< 0 ->
				 round(math:log(TotalNoFeatures)/math:log(2)) + 1;
			     X ->
				 X
			 end;
		     sqrt ->
			 round(math:sqrt(TotalNoFeatures))
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

    Conf = #rr_conf{cores = Cores,
		    score = Score,
		    prune = rr_tree:example_depth_stop(MinEx, MaxDepth),
		    evaluate = Eval,
		    progress = Progress,
		    split = fun rr_tree:random_split/3,
		    base_learner = {Classifiers, rr_tree},
		    no_features = TotalNoFeatures},

    io:format(standard_error, "*** Building model using ~p trees and ~p features *** ~n",
	      [Classifiers, NoFeatures]),

    Then = now(),
    io:format("*** Start ***"),
    RunExperiment(Features, Examples, Conf, Options),
    Time = timer:now_diff(now(), Then) / 1000000,
    
    io:format("~n** Parameters ** ~n"), 
    io:format("File: ~p ~n", [InputFile]),
    io:format("Trees: ~p ~n", [Classifiers]),
    io:format("Features: ~p ~n", [NoFeatures]),
    io:format("Time: ~p seconds ~n", [Time]),

    io:format(standard_error, "*** Model build in ~p second(s)*** ~n", [Time]),
    io:format("*** End ***~n").   

run_split(Features, Examples, Conf, Options) ->
    Split = get_opt(ratio, Options),
    io:format("~n** Split ~p ** ~n", [Split]),
    {Train, Test} = rr_example:split_dataset(Examples, Split),
    Model = rr_ensamble:generate_model(ordsets:from_list(Features), Train, Conf),
    Dict = rr_ensamble:evaluate_model(Model, Test, Conf),
    evaluate(Dict, rr_example:count(Test)).

run_cross_validation(Features, Examples, Conf, Options) ->
    Folds = get_opt(folds, Options),
    Avg = rr_example:cross_validation(
	    fun(Train0, Test0, Fold) ->
		    io:format(standard_error, "*** Fold ~p *** ~n", [Fold]),
		    io:format("~n** Fold: ~p ** ~n", [Fold]),
		    M = rr_ensamble:generate_model(ordsets:from_list(Features), Train0, Conf),
		    D = rr_ensamble:evaluate_model(M, Test0, Conf),
		    evaluate(D, rr_example:count(Test0))
	    end, Folds, Examples),
    io:format("~n** Fold average ** ~n"),
    lists:foreach(fun({accuracy, A}) ->
			  io:format("Accuracy: ~p ~n", [A]);
		     ({auc, A}) ->
			  io:format("Auc: ~p ~n", [A]);
		     ({brier, A}) ->
			  io:format("Brier: ~p ~n", [A])
		  end, average_cross_validation(Avg, Folds, [accuracy, auc, brier], [])).

average_cross_validation(_, _, [], Acc) ->
    lists:reverse(Acc);
average_cross_validation(Avg, Folds, [H|Rest], Acc) ->
    A = lists:foldl(fun (Measures, Sum) ->
			    case lists:keyfind(H, 1, Measures) of
				{H, _, Auc} ->
				    Sum + Auc;
				{H, O} ->
				    Sum + O
			    end
		    end, 0, Avg) / Folds,
    average_cross_validation(Avg, Folds, Rest, [{H, A}|Acc]).
		    

evaluate(Dict, NoTestExamples) ->
    Accuracy = rr_eval:accuracy(Dict),
    io:format("Accuracy: ~p ~n", [Accuracy]),

    Auc = rr_eval:auc(Dict, NoTestExamples),
    io:format("Area under ROC~n"),
    lists:foreach(fun({Class, A}) ->
			  io:format(" * ~s: ~p ~n", [Class, A])
		  end, Auc),
    AvgAuc = lists:foldl(fun({_, P}, A) -> A + P / length(Auc) end, 0, Auc),
    io:format(" average: ~p ~n", [AvgAuc]),
    
    io:format("Precision~n"),
    Precision = rr_eval:precision(Dict),
    lists:foreach(fun({Class, P}) ->
			  io:format(" * ~s: ~p ~n", [Class, P])
		  end, Precision),

    Brier = rr_eval:brier(Dict, NoTestExamples),
    io:format("Brier: ~p ~n", [Brier]),
    [{accuracy, Accuracy}, {auc, Auc, AvgAuc}, {precision, Precision}, {brier, Brier}].
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

Written by ~s ~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR, ?AUTHOR]).
