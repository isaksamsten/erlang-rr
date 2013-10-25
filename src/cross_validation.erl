%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for performing cross validation
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(cross_validation).

%% @headerfile "rr.hrl"
-include("rr.hrl").

-export([
         %% rr_evaluator behaviour
         evaluate/2,

         average_cross_validation/3,

         %% rr_command behaviour
         help/0,
         args/1,
         parse_args/1
        ]).

-behaviour(rr_command).
-behaviour(rr_evaluator).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CMD_SPEC, 
        [{<<"folds">>, $f, "folds", {integer, 10},
          "Number of cross validation folds"},
         {<<"progress">>, $p, "progress", {string, "default"},
          "Progress bar when running cross validation"}
        ]).
-define(NAME, "cv").

help() ->
    rr:show_help(options, ?CMD_SPEC, "cv").

parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

args(Args) ->
    args(Args, fun (Value, Reason) -> throw({bad_arg, ?NAME, Value, Reason}) end).

args(Args, Error) ->
    Folds = args(<<"folds">>, Args, Error),
    Progress = args(<<"progress">>, Args, Error),
    [{folds, Folds},
     {progress, Progress}].

args(Key, Args, Error) ->
    Value = proplists:get_value(Key, Args),
    case Key of
        <<"folds">> ->
            Value;
        <<"progress">> ->
            progress(Value, Error);
        _ ->
            Error("cv", Key) 
    end.

progress(_Value, _Error) ->
    fun (Fold) -> 
            io:format(standard_error, "fold ~p ", [Fold])
    end.

%% @doc
%% Perform cross-validation on examples
%% @end
%-spec evaluate(example_set(), any()) -> result_set().
evaluate(Dataset, Props) ->
    Build = case proplists:get_value(build, Props) of
                undefined -> throw({badarg, build});
                Build0 -> Build0
            end,
    Evaluate = case proplists:get_value(evaluate, Props) of
                   undefined -> throw({badarg, evaluate});
                   Evaluate0 -> Evaluate0
               end,
    NoFolds = proplists:get_value(folds, Props, 10),
    Average = proplists:get_value(average, Props, fun average_cross_validation/2),
    Progress = proplists:get_value(progress, Props, fun (_) -> ok end),
    Module = Dataset#dataset.module,
    Total0 = cross_validation(
               fun (Train, Test, Fold) ->
                       Progress(Fold),
                       Model = Build(Train),
                       Result = Evaluate(Model, Test),
                       {{{fold, Fold}, Result}, Model}                       
               end, NoFolds, Module, Dataset),
    {Total, Models} = lists:unzip(Total0),
    Avg = Average(Total, NoFolds),
    {{cv, NoFolds, Total ++ [Avg]}, Models}.

%% @doc stratified cross validation
cross_validation(Fun, NoFolds, Module, Dataset) ->
    Folds = Module:split(Dataset, {folds, NoFolds}),
    cross_validation(Fun, Folds, NoFolds, NoFolds, Module, []).

%% @private
cross_validation(_Fun, _Folds, _NoFolds, 0, _, Results) -> 
    lists:reverse(Results);
cross_validation(Fun, Folds, NoFolds, CurrentFold, Module, Results) -> 
    {Test, Train} = merge_folds(Folds, CurrentFold),
    Result = Fun(Module:merge(Train), Test, NoFolds - CurrentFold + 1),
    cross_validation(Fun, Folds, NoFolds, CurrentFold - 1, Module, [Result|Results]).

%% @private
init_folds(Folds, Init, Test) ->
    Init0 = dict:fetch(Init, Folds),
    TestSet0 = dict:fetch(Test, Folds),
    Rest0 = dict:erase(Init, Folds),
    {Init0, TestSet0, dict:erase(Test, Rest0)}.

%% @doc merge all folds in Folds expect for test
-spec merge_folds(dict(), integer()) -> {Test::examples(), Train::examples()}.
merge_folds(Folds, Test) ->
    {Init, TestSet, Rest} = case Test of
                                1 -> 
                                    init_folds(Folds, 2, Test);
                                _Other ->
                                    init_folds(Folds, 1, Test)
                            end,
    Train = dict:fold(fun (_, Fold, Acc) -> [Fold|Acc] end, [Init], Rest),
    {TestSet, Train}.

%% @private default method for averaging the results of cross-validation
average_cross_validation(Result, Folds) ->
    average_cross_validation(Result, Folds, [accuracy, auc, strength, correlation, c_s2, precision, recall,
                                             variance, oob_base_accuracy, base_accuracy, brier, no_rules]).

%% @doc average the cross-validation (for the results specified in Inputs)
average_cross_validation(Result, Folds, Inputs) ->
        average_cross_validation(Result, Folds, Inputs, []).

%% @private average cross-validation
average_cross_validation(_, _, [], Acc) ->
    {{fold, average}, lists:reverse(Acc)};
average_cross_validation(Avg, Folds, [H|Rest], Acc) ->
    A = lists:foldl(fun ({_, Measures}, Sum) ->
                            case lists:keyfind(H, 1, Measures) of
                                {H, {Tag, List, Auc}} ->
                                    case Sum of
                                        0 ->
                                            {Tag, average_list_item(List, Folds, 0), Sum + Auc/Folds};
                                        {Tag, ListSum, AvgSum} ->
                                            {Tag, average_list_item(List, Folds, ListSum), AvgSum + Auc/Folds}
                                    end;
                                {H, List} when is_list(List) ->
                                    average_list_item(List, Folds, Sum);
                                {H, O} ->
                                    Sum + O/Folds;
                                false ->
                                    Sum 
                            end
                    end, 0, Avg),
    average_cross_validation(Avg, Folds, Rest, [{H, A}|Acc]).

%% @doc average a list of items
average_list_item(List, Folds, 0) ->
    average_list_item(List, Folds, lists:map(
                                     fun ({Class, {_, _}}) -> {Class, {0, 0}};
                                         ({Class, _}) -> {Class, 0}
                                     end, List));
average_list_item([], _, Acc) -> Acc;
average_list_item([Item|Rest], Folds, Acc) ->
    NewAcc = case Item of
                 {Key, {_, 'n/a'}} -> % note: don't count when we got no value
                     case lists:keytake(Key, 1, Acc) of
                         {value, {Key, {_, B}}, AccRest} ->
                             [{Key, {0, B + 0 / Folds}}|AccRest];
                         false ->
                             [{Key, {0,  1/Folds}}|Acc]
                     end;
                 {Key, {_, A}} ->
                     case lists:keytake(Key, 1, Acc) of
                         {value, {Key, {_, B}}, AccRest} ->
                             [{Key, {0, B + A / Folds}}|AccRest];
                         false ->
                             [{Key, {0,  A/Folds}}|Acc]
                     end;
                 {Key, 'n/a'} ->
                     case lists:keytake(Key, 1, Acc) of
                         {value, {Key, B}, AccRest} ->
                             [{Key, B + 0 / Folds}|AccRest];
                         false ->
                             [{Key, 1/Folds}|Acc]
                     end;
                 {Key, A} ->
                     case lists:keytake(Key, 1, Acc) of
                         {value, {Key, B}, AccRest} ->
                             [{Key, B + A / Folds}|AccRest];
                         false ->
                             [{Key, A/Folds}|Acc]
                     end
             end,
    average_list_item(Rest, Folds, NewAcc).


-ifdef(TEST).

setup() ->
    rr_config:init([{'rr.classifiers', [{"rf", rf, "he"}]}]).

tear(_) ->
    rr_config:stop().

command_test_() ->
    {setup,
     fun setup/0,
     fun tear/1,
     [?_test(test_simple())]}.

test_simple() ->
    Opts = parse_args(["-f", "10"]),
    Args = args(Opts, fun(_, _) -> ok end),
    ?assertEqual(10, proplists:get_value(folds, Args)),
    ?assertEqual(true, is_function(proplists:get_value(progress, Args))).

classification_dataset_test() ->
    Dataset = classification_dataset:load(csv:binary_reader("../data/iris.txt")),
    Module = classification_dataset,
    Result = cross_validation(fun (Train, Test, _) ->
                                 {Module:no_examples(Train), Module:no_examples(Test)}
                         end, 10, Module, Dataset),
    ?assertEqual(10, length(Result)).


-endif.
