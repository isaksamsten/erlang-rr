%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(random_tree).
-behaviour(classifier).

%% TODO: insert the rf.hrl stuff here - its only used in this module.... 
%% @headerfile "rf.hrl"
-include("rf.hrl").

-export([
         %% classifier
         new/1,
         build/2,
         evaluate/2,
         
         kill/1,
         serialize/1,
         unserialize/1,

         %% model
         evaluate_model/4,
         predict/5,
         
         %% split strategies
         random_split/5,
         deterministic_split/5,
         value_split/5,

         %% prune
         example_depth_stop/2,
         chisquare_prune/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% todo: refactor away rr_example dependency (and use a dataset instead)

new(Props) ->
    LogFeatures = fun (T) -> trunc(math:log(T)/math:log(2)) + 1 end,
    NoFeatures = proplists:get_value(no_features, Props,
                                     LogFeatures),                 
    Missing = proplists:get_value(missing_values, Props, 
                                  fun rf_missing:weighted/6),
    Score = proplists:get_value(score, Props, fun rr_estimator:info_gain/2),
    Prune = proplists:get_value(pre_prune, Props, example_depth_stop(2, 1000)),
    FeatureSampling = proplists:get_value(feature_sampling, Props,
                                          fun subset/5),
    Distribute = proplists:get_value(distribute, Props,
                                     fun distribute/3),
    Split = proplists:get_value(split, Props, fun random_split/5),
    #rf_tree{
       score = Score,
       prune = Prune,
       branch = FeatureSampling,
       split = Split,
       distribute = Distribute,
       missing_values = Missing,
       no_features = NoFeatures
      }.

build(RandomTree, Dataset) ->
    Features = dataset:features(Dataset),
    Examples = dataset:examples(Dataset),
    Info = rr_estimator:info(Examples, dataset:no_examples(Dataset)),
    Model = build_decision_node(Features, Examples, dict:new(), 0, Info, Dataset, RandomTree, 1, 1),
    Model.%%    classifier:update(RandomTree, Model).

evaluate(RandomTree, Dataset) ->
    RandomTree,
    Dataset.

%% @doc has no effect
kill(_) -> ok.

%% @doc serialize this random tree
serialize(RandomTree) ->
    rr_system:serialize_model(RandomTree).

%% @doc unserialize this random tree
unserialize(Dump) ->
    Dump.

%% @doc prune if to few examples or to deep tree
-spec example_depth_stop(integer(), integer()) -> prune_fun().
example_depth_stop(MaxExamples, MaxDepth) ->
    fun(Examples, Depth) ->
            (Examples =< MaxExamples) orelse (Depth > MaxDepth)
    end.

%% @doc pre-prune if the split is not significantly better than no split
chisquare_prune(Sigma) ->
    fun (Split, Examples, Total) ->
            K = rr_estimator:chisquare(Split, Examples, Total),
            K < Sigma
    end.

evaluate_model(Model, Examples, ExConf, Conf) ->
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
                        predict_all(Class, ExampleIds, Model, ExConf, Conf, Acc)
                end, dict:new(), Examples).

%% @private
predict_all(_, [], _, _ExConf, _Conf, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, ExConf, Conf, Dict) ->
    {Prediction, _NodeNr} = predict(Example, Model, ExConf, Conf, []),
    predict_all(Actual, Rest, Model, ExConf, Conf,
                dict:update(Actual, fun (Predictions) ->
                                            [{Prediction, 0}|Predictions] 
                                    end, [{Prediction, 0}], Dict)). %% note: no other prob (fix?)

%% @doc predict an example according to a decision tree
-spec predict(ExId::exid(), tree(), #rr_example{},  #rf_tree{}, []) -> prediction().
predict(_, #rf_leaf{id=NodeNr, class=Class, score=Score}, _ExConf, _Conf, Acc) ->
    {{Class, Score, []}, [NodeNr|Acc]};
predict(ExId, Node, ExConf, Conf, Acc) ->
    #rf_node { 
       id=NodeNr, 
       feature=F, 
       distribution={LeftExamples, RightExamples, {Majority, Count}},
       left=Left, 
       right=Right} = Node,
    #rf_tree{distribute=Distribute, missing_values=Missing} = Conf,
    NewAcc = [NodeNr|Acc],
    case Distribute(ExConf, F, ExId) of
        {'?', _} ->
            case Missing(predict, ExConf, F, ExId, LeftExamples, RightExamples) of
                {left, _} ->
                    predict(ExId, Left, ExConf, Conf, NewAcc);
                {right, _} ->
                    predict(ExId, Right, ExConf, Conf, NewAcc);
                ignore ->
                    {{Majority, laplace(Count, LeftExamples+RightExamples)}, NewAcc}
            end;
        {left, _} ->
            predict(ExId, Left, ExConf, Conf, NewAcc);
        {right, _} ->
            predict(ExId, Right, ExConf, Conf, NewAcc)
    end.


majority(Examples) ->
    {Class, Count, _} = rr_util:max(fun({_, Count, _}) -> Count end, Examples),
    {Class, Count}.

count(Examples) ->
    lists:foldl(fun({_, Count, _}, Old) -> Count + Old end, 0, Examples).

%% @private create a node
make_node(Id, Feature, Dist, Score, Left, Right) ->
    #rf_node{id = Id, score=Score, feature=Feature, distribution=Dist, left=Left, right=Right}.

%% @private create a leaf
make_leaf(Id, [], Class) ->
    #rf_leaf{id=Id, score=0, distribution={0, 0}, class=Class};
make_leaf(Id, Covered, {Class, C}) ->
    N = count(Covered),
    #rf_leaf{id=Id, score=laplace(C, N), distribution={C, N-C}, class=Class}.

%% @private induce a decision tree
build_decision_node([], [], Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, [], error), Importance, Total, NoNodes};
build_decision_node([], Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, Examples, majority(Examples)), Importance, Total, NoNodes};
build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, Examples, {Class, Count}), Importance, Total, NoNodes};
build_decision_node(Features, Examples, Importance, Total, Error, ExConf, Conf, Id, NoNodes) ->
    #rf_tree{prune=Prune, pre_prune = _PrePrune, branch=Branch, depth=Depth} = Conf,
    NoExamples = count(Examples),
    case Prune(NoExamples, Depth) of
        true ->
            {make_leaf(Id, Examples, majority(Examples)), Importance, Total, NoNodes};
        false ->
            case rf_branch:unpack(Branch(Features, Examples, NoExamples, ExConf, Conf)) of
                no_information ->
                    {make_leaf(Id, Examples, majority(Examples)), Importance, Total, NoNodes};
                #candidate{split={_, _}} ->
                    {make_leaf(Id, Examples, majority(Examples)), Importance, Total, NoNodes};
                #candidate{feature={Feature, _} = FeatureValue, 
                           score={Score, LeftError, RightError}, 
                           split={both, LeftExamples, RightExamples}}  -> 
                    NewReduction = Error - (LeftError + RightError),
                    NewImportance = dict:update_counter(feature:id(Feature), NewReduction, Importance),

                    {LeftNode, LeftImportance, TotalLeft, NoLeftNodes} = 
                        build_decision_node(Features, LeftExamples, NewImportance, Total + NewReduction, LeftError, 
                                            ExConf, Conf#rf_tree{depth=Depth + 1}, Id + 1, NoNodes),

                    {RightNode, RightImportance, TotalRight, NoRightNodes} = 
                        build_decision_node(Features, RightExamples, LeftImportance, TotalLeft, RightError, 
                                            ExConf, Conf#rf_tree{depth=Depth + 1}, Id + 2, NoLeftNodes),
                    Distribution = {count(LeftExamples), count(RightExamples), majority(Examples)},
                    {make_node(Id, FeatureValue, Distribution, Score, LeftNode, RightNode), RightImportance, TotalRight, NoRightNodes+1}
            end    
    end.

%% @private
laplace(C, N) ->
    (C+1)/(N+2). %% NOTE: no classes?

%% @doc evaluate a subset of n random features
subset(Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score, 
       split = Split, 
       distribute = Distribute, 
       missing_values = Missing,
       no_features = NoFeatures
      } = Conf,
    NewFeatures = rr_example:random_features(Features, NoFeatures(length(Features))),
    {best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing), NewFeatures}.

%% @doc randomly split data set
random_split(ExConf, Feature, Examples, Distribute, Missing) ->
    split(ExConf, Feature, Examples, Distribute, Missing).

%% @doc sample a split-value from examples with values for the feature
value_split(ExConf, Feature, Examples, Distribute, Missing) ->
    split(ExConf, Feature, Examples, Distribute, Missing,
          fun (_, {numeric, _FeatureId}, _Ex) ->
                  none; %% TODO: sample from those with value
              (_, {categoric, _FeatureId}, _Ex) ->
                  none; %% TODO: same..
              (Me, Ff, Ex) ->
                  sample_split_value(Me, Ff, Ex)
          end).

%% @doc make a determinisc split in the numeric data set
deterministic_split(ExConf, Feature, Examples, Distribute, Missing) ->
    split(ExConf, Feature, Examples, Distribute, Missing, 
          fun (Me, {numeric, FeatureId}, Ex) ->
                  find_numeric_split(Me, FeatureId, Ex, fun rr_estimator:info/2);
              (Me, Ff, Ex) ->
                  sample_split_value(Me, Ff, Ex)
          end).

%% @private format the left and right distribution
-spec format_left_right_split([examples()], [examples()]) -> split().
format_left_right_split([], Right) ->
    {right, Right};
format_left_right_split(Left, []) ->
    {left, Left};
format_left_right_split(Left, Right) ->
    {both, Left, Right}.

%% @private distribute missing values either right or left depending on Distribute
distribute_missing_values(_, _, _, _, _, [], [], [], Left, Right, _) ->
    format_left_right_split(Left, Right);
distribute_missing_values(Me, Feature, Examples, TotalNoLeft, TotalNoRight, [Left|LeftRest], [Right|RightRest], 
                          [{_, _, Missing}|MissingRest], LeftAcc, RightAcc, Distribute) ->
    case  distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, Missing, Left, Right, Distribute) of
        {{_, 0, []}, NewRight} ->
            distribute_missing_values(Me, Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest,
                                      LeftAcc, [NewRight|RightAcc], Distribute);
        {NewLeft, {_, 0, []}} ->
            distribute_missing_values(Me, Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest, 
                                      [NewLeft|LeftAcc], RightAcc, Distribute);
        {NewLeft, NewRight} ->
            distribute_missing_values(Me, Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest, 
                                      [NewLeft|LeftAcc], [NewRight|RightAcc], Distribute)
    end.
            
distribute_missing_values_for_class(_, _, _, _, _, [], Left, Right, _) ->
    {Left, Right};
distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, [MissingEx|RestMissing], 
                                   {Class, NoLeft, Left} = LeftExamples, 
                                   {Class, NoRight, Right} = RightExamples, Distribute) ->
    case Distribute(build, Me, Feature, MissingEx, TotalNoLeft, TotalNoRight) of
        {right, {_, NewCount}=NewEx} ->
            distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, LeftExamples,
                                                {Class, NoRight + NewCount, [NewEx|Right]}, Distribute);
        {left, {_, NewCount}=NewEx} ->
            distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, 
                                                {Class, NoLeft + NewCount, [NewEx|Left]}, RightExamples, Distribute);
        {both, {{_, NewLeftCount}=NewLeftEx, {_, NewRightCount} = NewRightEx}} ->
            distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing,
                                                {Class, NoLeft + NewLeftCount, [NewLeftEx|Left]},
                                                {Class, NoRight + NewRightCount, [NewRightEx|Right]}, Distribute);
        ignore ->
            distribute_missing_values_for_class(Me, Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, 
                                                LeftExamples, RightExamples, Distribute)
    end.

%% @doc unpack a split to a tuple containing {Left, Right}
-spec unpack_split(split()) -> {examples() | [], examples() | []}.
unpack_split({both, Left, Right}) ->
    {Left, Right};
unpack_split({left, Left}) ->
    {Left, []};
unpack_split({right, Right}) ->
    {[], Right}.

split_feature_value(Me, FeatureValue, Examples, Distribute, DistributeMissing) ->
    {Left, Right, Missing} = split_feature(Me, FeatureValue, Examples, Distribute, [], [], []),
    distribute_missing_values(Me, FeatureValue, Examples, count(Left), count(Right), 
                              Left, Right, Missing, [], [], DistributeMissing).    

%% @doc Split Examples into two disjoint subsets according to Feature.
-spec split(#rr_example{}, feature(), examples(), distribute_fun(), missing_fun(), any()) -> {'$none' | atom(), split()}.
split(Me, Feature, Examples, Distribute, DistributeMissing, Sample) ->
    {Value, {Left, Right, Missing}} = split_with_value(Me, Feature, Examples, Distribute, Sample),
    {Value, distribute_missing_values(Me, {Feature, Value}, Examples, count(Left), count(Right), 
                                      Left, Right, Missing, [], [], DistributeMissing)}.

%% @doc split examples into two subsets according to feature handle split randomly
-spec split(#rr_example{}, feature(), examples(), distribute_fun(), missing_fun()) -> {'$none' | atom(), split()}.
split(Me, Feature, Examples, Distribute, DistributeMissing) ->
    split(Me, Feature, Examples, Distribute, DistributeMissing, fun sample_split_value/3).
    
%% @private Split into three disjoint subsets, Left, Right and Missing
split_with_value(Me, Feature, Examples, Distribute, Sample) ->
    case Sample(Me, Feature, Examples) of
        '$none' -> 
            {'$none', split_feature(Me, Feature, Examples, Distribute, [], [], [])};
        Value ->
            {Value, split_feature(Me, {Feature, Value}, Examples, Distribute, [], [], [])}
    end.

%% @private split the class distribution (i.e. one example())
split_class_distribution(_Me, _, [], _, _, Left, Right, Missing) ->
    {Left, Right, Missing};
split_class_distribution(Me, Feature, [ExampleId|Examples], Distribute, Class, 
                         {Class, NoLeft, Left} = LeftExamples, 
                         {Class, NoRight, Right} = RightExamples,
                         {Class, NoMissing, Missing} = MissingExamples) ->
    {NewLeftExamples, NewRightExamples, NewMissingExamples} = 
        case Distribute(Me, Feature, ExampleId) of
            {'?', Count} ->
                {LeftExamples, 
                 RightExamples, 
                 {Class, NoMissing + Count, [ExampleId|Missing]}};
            {left, Count} ->
                {{Class, NoLeft + Count, [ExampleId|Left]}, 
                 RightExamples, 
                 MissingExamples};
            {right, Count} ->
                {LeftExamples, 
                 {Class, NoRight + Count, [ExampleId|Right]}, 
                 MissingExamples};
            {left, {_, NewNo} = NewEx, {_, NewNoMissing} = NewMissingEx} ->
                {{Class, NoLeft + NewNo, [NewEx|Left]}, 
                 RightExamples, 
                 {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
            {right, {_, NewNo} = NewEx, {_, NewNoMissing} = NewMissingEx} ->
                {LeftExamples, {Class, NoLeft + NewNo, [NewEx|Right]}, 
                 {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
            {all, 
             {_, NewNoLeft} = NewLeftEx, 
             {_, NewNoRight} = NewRightEx, 
             {_, NewNoMissing} = NewMissingEx} ->
                {{Class, NoLeft + NewNoLeft, [NewLeftEx|Left]}, 
                 {Class, NoRight + NewNoRight, [NewRightEx|Right]},
                 {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
            {both, {_, NewNoLeft} = NewLeftEx, {_, NewNoRight} = NewRightEx} ->
                {{Class, NoLeft + NewNoLeft, [NewLeftEx|Left]},
                 {Class, NoRight + NewNoRight, [NewRightEx|Right]},
                 MissingExamples}
        end,
    split_class_distribution(Me, Feature, Examples, Distribute, Class, 
                             NewLeftExamples, NewRightExamples, NewMissingExamples).

%% @doc default function for distributing examples left or right
-spec distribute(#rr_example{}, Feature::feature(), exid()) -> distribute_example().
distribute(Database, {{categoric, FeatureId}, SplitValue}, ExId) ->
    {case dataset:value(Database, ExId, FeatureId) of
        '?' -> '?';
        Value when Value == SplitValue -> left;
        _ -> right
    end, example:count(ExId)};
distribute(Database, {{numeric, FeatureId}, Threshold}, ExId) ->
    {case dataset:value(Database, ExId, FeatureId) of
        '?' -> '?';
        Value when Value >= Threshold -> left;
        _ -> right
    end, example:count(ExId)};
distribute(Database, {{combined, FeatureA, FeatureB}, 
                {combined, SplitValueA, SplitValueB}}, ExId) ->
    {A, _} = distribute(Database, {FeatureA, SplitValueA}, ExId),
    {B, C} = distribute(Database, {FeatureB, SplitValueB}, ExId),
    {case {A, B} of
        {'?', B} ->
            B;
        {A, '?'} ->
            A;
         {'?', '?'} ->
             '?';
        {A, B} when A == B ->
            A;
        {A, B} when A =/= B ->
            Rand = random:uniform(),
            if Rand >= 0.5 ->
                    A;
               true ->
                    B
            end
     end, C};
distribute(Database, {rule, Rule, _Lenght}, ExId) ->
    {rf_rule:evaluate_rule(Database, Rule, ExId), example:count(ExId)}.

%% @private split data set using Feature
split_feature(_Me, _Feature, [], _, Left, Right, Missing) ->
    {Left, Right, Missing};
split_feature(Me, Feature, [{Class, _, ExampleIds}|Examples], 
              Distribute, Left, Right, Missing) ->
    case split_class_distribution(Me, Feature, ExampleIds, Distribute, Class, 
                                  {Class, 0, []}, {Class, 0, []}, {Class, 0, []}) of
        {LeftSplit, RightSplit, MissingSplit} ->
            split_feature(Me, Feature, Examples, Distribute, 
                          [LeftSplit|Left], 
                          [RightSplit|Right], 
                          [MissingSplit|Missing])
    end.

%% @private find the best numeric split point
find_numeric_split(Me, FeatureId, Examples, Gain) ->
    case lists:keysort(1, lists:foldl(
                            fun ({Class, _, ExIds}, NewIds) ->
                                    lists:foldl(fun(ExId, Acc) ->
                                                        case dataset:value(Me, ExId, FeatureId) of
                                                            '?' -> Acc;
                                                            Feature -> [{Feature, Class}|Acc]
                                                        end
                                                end, NewIds, ExIds)
                            end, [], Examples)) of
        [{Value, Class}|ClassIds] ->
            Gt = lists:map(fun({C, Num, _}) -> {C, Num, []} end, Examples),
            Lt = lists:map(fun({C, _, _}) -> {C, 0, []} end, Examples),
            Dist = {both, Lt, Gt},
            First = {Value, Class},
            Total = rr_example:count(Examples),
            find_numeric_split(ClassIds, First, FeatureId, Gain, Total, {Value/2, inf}, Dist);
        [] ->
            0.0 %% note: all values are missing
    end.

find_numeric_split([], _, _, _, _, {Threshold, _}, _) ->
    Threshold;
find_numeric_split([{Value, Class}|Rest], {OldValue, OldClass}, FeatureId, 
                            Gain, Total, {OldThreshold, OldGain}, Dist) ->
    {both, Left, Right} = Dist, 
    Dist0 = case lists:keytake(Class, 1, Left) of
                {value, {Class, Num, _}, ClassRest} ->
                    {both, [{Class, Num + 1, []}|ClassRest], Right}
            end,
    {both, Left0, Right0} = Dist0,
    NewDist = case lists:keytake(Class, 1, Right0) of
        {value, {Class, Num0, _}, ClassRest0} ->
            {both, Left0, [{Class, Num0 - 1, []}|ClassRest0]}
    end,
    case Class == OldClass of
        true -> find_numeric_split(Rest, {Value, Class}, FeatureId,
                                   Gain, Total, {OldThreshold, OldGain}, NewDist);
        false ->
            Threshold = (Value + OldValue) / 2,
            {NewGain0, _, _} = Gain(NewDist, Total),
            NewThreshold = case NewGain0 < OldGain of
                               true -> {Threshold, NewGain0};
                               false -> {OldThreshold, OldGain}
                           end,
            find_numeric_split(Rest, {Value, Class}, FeatureId,
                                        Gain, Total, NewThreshold, NewDist)
    end.

%% @doc 
%% sample a split point. this function is used in split() and can be overriden. 
%% please use this as the default
%% @end
sample_split_value(Me, Feature, Examples) ->
    case Feature of
         {categoric, FeatureId} ->
             resample_categoric_split(Me, FeatureId, Examples, 5);
         {numeric, FeatureId} ->
             sample_numeric_split(Me, FeatureId, Examples);
         {combined, A, B} ->
             sample_combined(Me, A, B, Examples);
         _ ->
            '$none'
     end.

sample_split_value(Me, Feature, Examples, Ex1, Ex2) ->
    case Feature of
        {categoric, FeatureId} ->
            dataset:value(Me, Ex1, FeatureId);
        {numeric, FeatureId} ->
            case sample_numeric_split(Me, FeatureId, Ex1, Ex2) of
                {'?', '?'} ->
                    0;
                X ->
                    X
            end;
        {combined, A, B} ->
            sample_combined(Me, A, B, Examples)
    end.

%% @private sample two features from the same example
sample_combined(Me, FeatureA, FeatureB, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    {combined, 
     sample_split_value(Me, FeatureA, Examples, Ex1, Ex2), 
     sample_split_value(Me, FeatureB, Examples, Ex1, Ex2)}.

%% @private sample a numeric split point
sample_numeric_split(Me, FeatureId, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    case sample_numeric_split(Me, FeatureId, Ex1, Ex2) of
        '?' ->
           '?';
        X ->
            X
    end.
sample_numeric_split(Me, FeatureId, Ex1, Ex2) ->
    Value1 = dataset:value(Me, Ex1, FeatureId),
    Value2 = dataset:value(Me, Ex2, FeatureId),
    case {Value1, Value2} of
        {'?', Value2} ->
            Value2;
        {Value1, '?'} ->
            Value1;
        {Value1, Value2} ->
            (Value1 + Value2) / 2;
        {'?', '?'} ->
            '?'
    end.

%% @private resample a random categoric split if a missing value is found
resample_categoric_split(_, _, _, 0) ->
    '?';
resample_categoric_split(Me, FeatureId, Examples, N) ->
    case sample_categoric_split(Me, FeatureId, Examples) of     
        '?' ->
            resample_categoric_split(Me, FeatureId, Examples, N - 1);
        X ->  
            X
    end.

%% @private sample a categoric split
sample_categoric_split(Me, FeatureId, Examples) ->
    ExId = sample_example(Examples),
    dataset:value(Me, ExId, FeatureId).

%% @doc find the best split from features
best_split(_, [], _, _, _, _, _, _) ->
    no_features;
best_split(Me, [F|Features], Examples, Total, Score, Split, Distribute, Missing) ->
    {T, ExSplit} = Split(Me, F, Examples, Distribute, Missing),
    Cand = #candidate{feature={F, T}, score=Score(ExSplit, Total), split=ExSplit},
    best_split(Me, Features, Examples, Total, Score, Split, Distribute, Missing, Cand).

best_split(_, [], _, _, _, _, _, _, Acc) ->
    Acc;
best_split(Me, [F|Features], Examples, Total, Score, Split, Distribute, Missing, OldCand) ->
    Cand = case Split(Me, F, Examples, Distribute, Missing) of
               {Threshold, ExSplit} ->
                   #candidate{feature = {F, Threshold}, 
                                 score = Score(ExSplit, Total), 
                                 split = ExSplit}                      
           end,
    best_split(Me, Features, Examples, Total, Score, Split, Distribute, Missing, 
               case element(1, Cand#candidate.score) < element(1, OldCand#candidate.score) of
                   true -> Cand;
                   false -> OldCand
               end).

%% @doc sample one example from all examples
sample_example([{_Class, _, ExIds}]) ->
    lists:nth(random:uniform(length(ExIds)), ExIds);
sample_example(Examples) ->
    sample_example([lists:nth(random:uniform(length(Examples)), Examples)]).

%% @doc sample two examples from different classes
sample_example_pair([{_, _, ExId1}, {_, _, ExId2}]) ->
    sample_example_pair(ExId1, ExId2);
sample_example_pair(Examples) ->
    sample_example_pair(sample_class_pair(Examples)).

%% @private
sample_example_pair(ExId1, ExId2) ->
    {lists:nth(random:uniform(length(ExId1)), ExId1),
     lists:nth(random:uniform(length(ExId2)), ExId2)}.

%% @private
sample_class_pair(Examples) ->
    NoEx = length(Examples),
    Random = random:uniform(NoEx),
    sample_class_pair(Examples, Random, NoEx, [lists:nth(Random, Examples)]).

%% @private
sample_class_pair(Examples, Random, NoEx, Acc) ->
    case random:uniform(NoEx) of
        Random ->
            sample_class_pair(Examples, Random, NoEx, Acc);
        Other ->
            [lists:nth(Other, Examples)|Acc]
    end.
-ifdef(TEST).

new_tree_test() ->
    Tree = new([]),
    Dataset = classification_dataset:load(csv:binary_reader("../data/iris.txt")),
    {M, I, _, _} = build(Tree, Dataset),
    ?debugFmt("~p ~n ~p ~n", [M, dict:to_list(I)]).


-endif.
