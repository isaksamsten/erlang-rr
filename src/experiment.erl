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
         help/0,

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
         {<<"tag">>, $t, "tag", {string, "experiment"},
          "Tag the experiment"},
         {<<"evaluator">>, $x, "evaluator", {string, "cv --folds 10"},
          "Evaluation settings."},
         {<<"classifier">>, $c, "classifier", string,
          "Classifier settings."}]).      
-define(NAME, "experiment").

help() ->
    rr:show_help(options, ?CMD_SPEC, "experiment").

parse_args(Args) ->
    io:format("~p ~n", [Args]),
    rr:parse(?NAME, Args, ?CMD_SPEC).

args(Args, Error) ->
    Iterations = args(<<"iterations">>, Args, Error),
    Evaluation = args(<<"evaluator">>, Args, Error),
    Classifier = args(<<"classifier">>, Args, Error),
    
    [{iterations, Iterations},
     {evaluator, Evaluation},
     {classifier, Classifier}].

args(Arg, Args, _Error) ->
    Value = proplists:get_value(Arg, Args),
    case Arg of
        <<"evaluator">> ->
            rr_evaluator:find(Value);
        <<"classifier">> ->
            rr_classifier:find(Value);
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
    Tag = args(<<"tag">>, Args, fun rr:illegal_option/2),
    Output = fun (Dataset, Iteration, Res) ->
                     Csv = rr_result:csv(
                             fun (info, Fold) ->
                                     io:format("fold ~p,", Fold),
                                     io:format("~s,iteration ~p,~s,", [Dataset, Iteration, Tag]);
                                 (value, Value) ->
                                     io:format("~p,", Value);
                                 (value_end, Value) ->
                                     io:format("~p~n", Value)
                             end),
                     Csv(Res)
             end,
    DatasetFolder = args(<<"dataset">>, Args, fun rr:illegal_options/2),
    Experiment = new(Options ++ 
                         [
                          {progress, Progress},
                          {output, Output}
                         ]),
    run(load_dir(DatasetFolder), Experiment),
    ok.

load_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            Files = lists:foldl(fun (File, Acc) ->
                                        case filename:extension(File) of
                                            ".txt" -> [filename:join(Dir, File)|Acc];
                                            _ -> Acc
                                        end
                                end, [], Filenames),
            rr_log:debug("running experiment with ~p files", [length(Files)]),
            lists:reverse(Files);
        _ ->
            rr:illegal("datasets", "could not find folder")
    end.

new(Props) ->
    Cores = proplists:get_value(cores, Props, erlang:system_info(schedulers)),
    Evaluate = proplists:get_value(evaluator, Props),
    Classifier = proplists:get_value(classifier, Props),
    Output = proplists:get_value(output, Props, fun (_,_,_) -> ok end),
    Iterations = proplists:get_value(iterations, Props, 10),
    Progress = proplists:get_value(progress, Props, fun (_, _) -> ok end),
    Loader = proplists:get_value(loader, Props, 
                                 fun (File) ->
                                         rr_example:load(csv:binary_reader(File), 1)
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
run([Dataset|Datasets], Evaluate, Classifier, Loader, 
    Output, Progress, Iterations, Acc) ->
    ExSet = Loader(Dataset),
    Result = lists:foldl(
               fun (Iteration, Results) ->
                       Progress(Dataset, {Iteration, Iterations}),
                       {Res, Models} = Evaluate(ExSet, Classifier),
                       Output(Dataset, Iteration, Res),
                       [{Iteration, Res, Models}|Results]
               end, [], lists:seq(1, Iterations)),
    run(Datasets, Evaluate, Classifier, Loader, 
        Output, Progress, Iterations, [{Dataset, Result}|Acc]).
                                         
                                                        
                                                            
