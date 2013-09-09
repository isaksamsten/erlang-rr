%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Usage: 
%%    Ex = experiment:new([....]),
%%    experiment:run([Datasets], [....])
%%% @end
%%% Created :  9 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(experiment).
-export([
	 new/1,
	 run/2,

	 main/1,
	 args/2,
	 args/3,
	 parse_args/1
	]).

-include("experiment.hrl").

-define(CMD_SPEC,
	[{<<"help">>, $h, "help", undefined,
	  "Show this usage information."},
	 {<<"version">>, $v, "version", undefined,
	  "Show the program version."},
	 {<<"dataset">>, $d, "datasets", string,
	  "Folder containing datasets"},
	 {<<"iterations">>, $i, "iterations", {integer, 10},
	  "Number of iterations to run each experiment"},
	 {<<"evaluation">>, $e, "evaluation", {string, "cv --folds 10"},
	  "Evaluation settings."},
	 {<<"classifier">>, $c, "classifier", {string, "rf -n 100"},
	  "Classifier settings."}]).	  

parse_args(Args) ->
    rr:parse(Args, ?CMD_SPEC).

args(Args, Error) ->
    Iterations = args(<<"iterations">>, Args, Error),
    Evaluation = args(<<"evaluation">>, Args, Error),
    Classifier = args(<<"classifier">>, Args, Error),
    
    [{iterations, Iterations},
     {evaluate, Evaluation},
     {classifier, Classifier}].

args(Arg, Args, Error) ->
    Value = proplists:get_value(Arg, Args),
    case Arg of
	<<"evaluation">> ->
	    evaluation(Value, Error);
	<<"classifier">> ->
	    classifier(Value, Error);
	_ ->
	    Value
    end.

main(Args) ->
    Options = args(Args, fun rr:illegal_option/2),
    Progress = fun (done, done) -> 
		       io:format(standard_error, "~n", []);
		   (Dataset, {I, Oi}) -> 
		       io:format(standard_error, 
				 "running ~s iteration ~p/~p~n", [Dataset, I, Oi])
	       end,
    Output = fun (Dataset, Iteration, Res) ->
		     Csv = rr_result:csv(
			     fun (info, Fold) ->
				     io:format("fold ~p,", Fold),
				     io:format("~s,~p,", [Dataset, Iteration]);
				 (value, Value) ->
				     io:format("~p,", Value);
				 (value_end, Value) ->
				     io:format("~p~n", Value)
			     end),
		     Csv(Res)
	     end,
    Experiment = new(Options ++ 
			 [
			  {progress, Progress},
			  {output, Output}
			 ]),
    run(["data/iris.txt", "data/car.txt"], Experiment),
    ok.

new(Props) ->
    Cores = proplists:get_value(cores, Props, erlang:system_info(schedulers)),
    Evaluate = proplists:get_value(evaluate, Props),
    Classifier = proplists:get_value(classifier, Props),
    Output = proplists:get_value(output, Props, fun (_,_,_) -> ok end),
    Iterations = proplists:get_value(iterations, Props, 10),
    Progress = proplists:get_value(progress, Props, fun (_, _) -> ok end),
    Loader = proplists:get_value(loader, Props, 
				 fun (File) ->
					 rr_example:load(csv:reader(File), Cores)
				 end),
    #experiment {
       evaluate = Evaluate,
       classifier = Classifier,
       loader = Loader, 
       output = Output, 
       progress = Progress, 
       iterations = Iterations
      }.

run(Datasets, Experiment) ->
    #experiment{
       evaluate = Evaluate, 
       classifier = Classifier,
       loader = Loader, 
       output = Output, 
       progress = Progress, 
       iterations = Iterations
      } = Experiment,
    run(Datasets, Evaluate, Classifier, Loader, Output, Progress, Iterations, []).
    

run([], _, _, _, _, Progress, _, Acc) ->
    Progress(done, done),
    Acc;
run([Dataset|Datasets], Evaluate, Classifier, Loader, Output, Progress, Iterations, Acc) ->
    ExSet = Loader(Dataset),
    Result = lists:foldl(
	       fun (Iteration, Results) ->
		       Progress(Dataset, {Iteration, Iterations}),
		       {Res, _Models} = Evaluate(ExSet, Classifier()),
		       Output(Dataset, Iteration, Res),
		       [Res|Results]
	       end, [], lists:seq(1, Iterations)),
    run(Datasets, Evaluate, Classifier, Loader, Output, Progress, Iterations, [Result|Acc]).
		       
evaluation(Value, _Error) ->    
    case string:tokens(Value, " ") of
	["cv"|_Cmd] ->
	     CvProgress = fun (Fold) -> 
				  io:format(standard_error, "fold ~p ", [Fold])
			  end,
	    fun (Dataset, Props) ->
		    cross_validation:evaluate(Dataset, Props ++ [{progress, CvProgress}])
	    end
    end.

classifier(Value, Error) ->
    case string:tokens(Value, " ") of
	["rf"|Cmd] ->
	    Args = rf:parse_args(Cmd),
	    Opts = rf:args(Args, Error),
	    Rf = rf:new(Opts),
	    Build = rf:partial_build(Rf),
	    Evaluate = rf:partial_evaluate(Rf),
	    fun () ->
		    [{build, Build}, {evaluate, rf:killer(Evaluate)}]
	    end
    end.


					 
							
							    
