-module(me).

-export([
         new/1,
         parse_args/1,
         args/1,

         help/0,

         test/0
        ]).

-include("rr.hrl").

                                                %-behaviour(rr_module).
-behaviour(rr_processor).

-define(CMD_SPEC,
        [{<<"bootstraps">>,  $k, "bootstraps", {integer, 5},
          "Specifies the number of bootstraps to average"},
         {<<"overlap">>, $o, "overlap", {float, 0.1},
          "Specifies the class overlap"}]).

-define(NAME, "me").

new(_) ->
    ok.

parse_args(_) ->
    ok.

args(_) ->
    ok.

help() ->
    ok.

fit(Features, Examples, Dataset, NoBootstraps, Overlap) ->
    MaxId = Dataset#rr_example.max_id,
    NoExamples = rr_example:count(Examples),
    %{TargetClass, _, _} = rr_util:min(fun ({_, M, _}) -> M end, Examples),
    TargetClass = 'Iris-setosa',
    Bootstraps = lists:map(
                   fun (_) ->
                           %% Todo: only bootstrap IN minority class
                           Bootstrap = rr_sampling:generate_bootstrap(NoExamples),
                           rr_sampling:select_bootstrap_examples(
                             Examples, select_examples(TargetClass, Bootstrap, Overlap))
                   end, lists:seq(1,NoBootstraps)),
    AverageExample = average_example(Features, Dataset, Bootstraps, MaxId),
    io:format("for target ~p ~n", [TargetClass]).

select_examples(TargetClass, Bootstrap, Overlap) ->
    fun (Class, ExId, _Total) ->
            if Class == TargetClass ->
                    dict:find(ExId, Bootstrap);
               true ->
                    Rand = random:uniform(),
                    if Rand < Overlap ->
                            dict:find(ExId, Bootstrap);
                       true ->
                            error
                    end
            end
    end.

average_example(Features, Dataset, Bootstraps, Id) ->
    FeatureVector = average_examples(Features, Dataset, Bootstraps, [Id]),
    io:format("~p ~n", [FeatureVector]),
    FeatureVector.

average_examples([], _Dataset, _Boot, Acc) ->
    list_to_tuple(lists:reverse(Acc));
average_examples([Feature|Features], Dataset, Bootstraps, Acc) ->
    Value = average_feature(Feature, Dataset, Bootstraps),
    average_examples(Features, Dataset, Bootstraps, [Value|Acc]).

average_feature({categoric, FId}, Dataset, Bootstraps) ->
    Values = 
        lists:foldl(
          fun ({InBag, _}, Dict) ->
                  fold_bootstrap(
                    fun ({ExId, Count}, Dict1) ->
                            Value = rr_example:feature(Dataset, ExId, FId),
                            dict:update_counter(Value, Count, Dict1)
                    end, Dict, InBag)
          end, dict:new(), Bootstraps),
    element(1, rr_util:max(fun ({_, V}) -> V end, dict:to_list(Values)));
average_feature({numeric, FId}, Dataset, Bootstraps) ->
    {Sum, Count} =
        lists:foldl(
          fun ({InBag, _}, Mean) -> %% NOTE: Ignoring missing values is
                  fold_bootstrap(   %% probably a bad idea..
                    fun ({ExId, Count}, {Mean1, Total}) ->
                            case rr_example:feature(Dataset, ExId, FId) of
                                '?' -> {Mean1, Total};
                                Value -> {Value*Count+Mean1, Total+Count}
                            end
                    end, Mean, InBag)
          end, {0.0, 0}, Bootstraps),
    Sum/Count.



fold_bootstrap(_Fun, Value, []) ->
    Value;
fold_bootstrap(Fun, Value, [{_, _, Bootstrap}|Rest]) ->
    NewValue = fold_bootstrap_for_class(Fun, Value, Bootstrap),
    fold_bootstrap(Fun, NewValue, Rest).

fold_bootstrap_for_class(_Fun, Value, []) ->
    Value;
fold_bootstrap_for_class(Fun, Value, [ExId|Rest]) ->
    NewValue = Fun(ExId, Value),
    fold_bootstrap_for_class(Fun, NewValue, Rest).

test() ->
    random:seed(now()),
    File = csv:binary_reader("data/iris.txt"),
    #rr_exset {
       features=Features, 
       examples=Examples, 
       exconf=Dataset
      } = Ds = rr_example:load(File, 4),
    io:format(" *** Start *** ~n"),
    Time = now(),
    NewExSet = fit(Features, Examples, Dataset, 100, 0.00),
    io:format(" *** End ~p *** ~n", [rr:seconds(Time)]).
