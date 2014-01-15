-module(smote).

-compile(export_all).
-include("rr.hrl").

fit(ExSet, Smote, K) ->
    #rr_exset {
       features = Features,
       examples = Examples,
       exconf = ExConf
      } = ExSet,
    MaxId = rr_example:count(Examples),
    NN = knn:fit(Features, Examples, ExConf, 4),
    {_, MaxCount, _} = rr_util:max(fun ({_, M, _}) -> M end, Examples),
    #rr_example{examples = ExDb} = ExConf,
    smote(Features, Examples, ExDb, NN, K, Smote, MaxId, MaxCount).

smote(Features, Examples, ExDb, NN, K, Smote, MaxId, MaxCount) ->
    smote_for_class(Features, Examples, ExDb, NN, K, Smote, MaxId, MaxCount, []).

smote_for_class(_, [], _, _, _, _, _, _, Acc) ->
    Acc;
smote_for_class(Features, [{Class, NoEx, Ex}|Rest], ExDb, 
                NN, K, Smote, MaxId, MaxCount, Acc) when NoEx < MaxCount ->
    SmoteEx = trunc(NoEx * Smote),
    NewNoEx = NoEx + SmoteEx,
    NewEx = smote_examples(Features, Ex, ExDb, NN, K, MaxId, []),
    NewAcc = [{Class, NewNoEx, NewEx}|Acc],
    smote_for_class(Features, Rest, ExDb, NN, K, Smote, MaxId + SmoteEx, MaxCount, NewAcc).

smote_examples(_, [], _ExDb, _NN, _K, _MaxId, Acc) ->
    Acc;
smote_examples(Features, [Ex|Rest], ExDb, NN, K, MaxId, Acc) ->
    NewId = MaxId + 1,
    KNearest = knn:pknearest(NN, Ex, K),
    insert_smote_example(Features, Ex, NewId, ExDb, KNearest),
    smote_examples(Features, Rest, ExDb, NN, K, NewId, [NewId|Acc]).

insert_smote_example(Features, OldEx, NewId, ExDb, KN) ->
    N = lists:nth(random:uniform(length(KN)), KN),
    FeatureVector = format_smote_example(Features, N, OldEx, ExDb, [NewId]),
    ets:insert(ExDb, FeatureVector).

format_smote_example([], _, _, _, Acc) ->
    list_to_tuple(lists:reverse(Acc));
format_smote_example([{Type, Axis}|Rest], OldEx, NEx, ExDb, Acc) ->
    Value = smote_value(Type, Axis, ExDb, OldEx, NEx),
    format_smote_example(Rest, OldEx, NEx, ExDb, [Value|Acc]).

smote_value(numeric, Axis, ExDb, OldEx, NEx) ->
    case {rr_example:feature(ExDb, OldEx, Axis), rr_example:feature(ExDb, NEx, Axis)} of
        {'?', _} ->
            '?';
        {_, '?'} ->
            '?';
        {OldValue, NewValue} ->
            Diff = NewValue - OldValue,
            Gap = random:uniform(),
            OldValue + Gap * Diff
    end;
smote_value(categoric, Axis, ExDb, OldEx, NEx) ->
    ok.


    
    

    
