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
    Prune = proplists:get_value(pre_prune, Props, 
                                rf_tree:example_depth_stop(2, 1000)),
    FeatureSampling = proplists:get_value(feature_sampling, Props,
                                          rf_branch:subset()),
    Distribute = proplists:get_value(distribute, Props,
                                     fun rr_example:distribute/3),
    Split = proplists:get_value(split, Props, fun rf_tree:random_split/5),
    {?MODULE, #classifier{
                 model = undefined,
                 config = #rf_tree{
                             score = Score,
                             prune = Prune,
                             branch = FeatureSampling,
                             split = Split,
                             distribute = Distribute,
                             missing_values = Missing,
                             no_features = NoFeatures
                            }}}.

build(RandomTree, Dataset) ->
    #dataset{target = class, module = Module} = Dataset,
    Info = rr_estimator:info(Module:examples(Dataset), Module:no_examples(Dataset)),
    Config = RandomTree#classifier.config,
    Model = build_node(Config, Dataset, dict:new(), 0, Info, 1, 1, 1),
    classifier:update(?MODULE, RandomTree, Model).

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

build_node(Config, Dataset, Importance, TotalError, Error, Depth, Id, NoNodes) ->
    #dataset{module = Module} = Dataset,
    #rf_tree{prune = Prune} = Config,
    case is_leaf(Module, Dataset, Prune, Depth) of
        {true, Majority} -> 
            {make_leaf(Id, Dataset, Majority), Importance, TotalError, NoNodes};
        false-> 
            make_subnode(Config, Module, Dataset, Importance, TotalError, Error, Depth, Id, NoNodes)
    end.

is_leaf(Module, Dataset, Prune, Depth) ->
    Examples = Module:examples(Dataset),
    Features = Module:features(Dataset),
    case {Features, Examples} of
        {[], []} -> {true, {undefined, undefined}};
        {[], Examples} -> {true, majority(Examples)};            
        {_, [{Class, Count, _}]} -> {true, {Class, Count}};
        _ -> 
            case Prune(Module:no_examples(Dataset), Depth) of
                true -> {true, majority(Examples)};
                false -> false
            end
    end.

make_subnode(Config, Module, Dataset, Importance, TotalError, Error, Depth, Id, NoNodes) ->
    #rf_tree{branch = Branch} = Config,
    case find_split(Branch, Config, Dataset) of
        {leaf, Majority} -> make_leaf(Id, Dataset, Majority);
        #candidate{
           feature = Feature,
           score = {Score, LeftError, RightError},
           split = {both, LeftExamples, RightExamples}} ->
            Reduction = Error - (LeftError + RightError),
            NewImportance = dict:update_counter(feature:id(Feature), Reduction, Importance),
            {LeftNode, LeftImportance, TotalLeft, NoLeftNodes} = 
                build_node(Config, LeftExamples, NewImportance, LeftError, TotalError + Reduction, 
                           Depth + 1, Id + 1, NoNodes),
            {RightNode, RightImportance, TotalRight, NoRightNodes} = 
                build_node(Config, RightExamples, LeftImportance, TotalLeft, RightError, 
                           Depth + 1, Id + 2, NoLeftNodes),
            Distribution = {
              Module:no_examples(LeftExamples),
              Module:no_examples(RightExamples),
              majority(Module:examples(Dataset))
             },
            {make_node(Id, Feature, Score, Distribution, LeftNode, RightNode),
             RightImportance, TotalRight, NoRightNodes + 1}
    end.

find_split(Branch, Config, Dataset) ->
    #dataset{module = Module} = Dataset,
    Examples = Module:examples(Dataset),
    case Branch(Dataset, Config) of
        false -> {leaf, majority(Examples)};           
        #candidate{split = {_, _}} -> {leaf, majority(Examples)};
        Candidate -> Candidate
    end.            

majority(Examples) ->
    {Class, Count, _} = rr_util:max(fun({_, Count, _}) -> Count end, Examples),
    {Class, Count}.

%% @private create a node
make_node(Id, Feature, Dist, Score, Left, Right) ->
    #rf_node{id = Id, score=Score, feature=Feature, distribution=Dist, left=Left, right=Right}.

%% @private create a leaf
make_leaf(Id, [], Class) ->
    #rf_leaf{id=Id, score=0, distribution={0, 0}, class=Class};
make_leaf(Id, Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rf_leaf{id=Id, score=laplace(C, N), distribution={C, N-C}, class=Class}.


            
%% %% @private induce a decision tree
%% build_decision_node([], [], Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
%%     {make_leaf(Id, [], error), Importance, Total, NoNodes};
%% build_decision_node([], Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
%%     {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, NoNodes};
%% build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
%%     {make_leaf(Id, Examples, {Class, Count}), Importance, Total, NoNodes};
%% build_decision_node(Features, Examples, Importance, Total, Error, ExConf, Conf, Id, NoNodes) ->
%%     #rf_tree{prune=Prune, pre_prune = _PrePrune, branch=Branch, depth=Depth} = Conf,
%%     NoExamples = rr_example:count(Examples),
%%     case Prune(NoExamples, Depth) of
%%         true ->
%%             {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, NoNodes};
%%         false ->
%%             case rf_branch:unpack(Branch(Features, Examples, NoExamples, ExConf, Conf)) of
%%                 no_information ->
%%                     {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, NoNodes};
%%                 #rr_candidate{split={_, _}} ->
%%                     {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, NoNodes};
%%                 #rr_candidate{feature=Feature, 
%%                               score={Score, LeftError, RightError}, 
%%                               split={both, LeftExamples, RightExamples}}  -> 
%%                     NewReduction = Error - (LeftError + RightError),
%%                     NewImportance = dict:update_counter(rr_example:feature_id(Feature), NewReduction, Importance),
                    
%%                     {LeftNode, LeftImportance, TotalLeft, NoLeftNodes} = 
%%                         build_decision_node(Features, LeftExamples, NewImportance, Total + NewReduction, LeftError, 
%%                                             ExConf, Conf#rf_tree{depth=Depth + 1}, Id + 1, NoNodes),
                    
%%                     {RightNode, RightImportance, TotalRight, NoRightNodes} = 
%%                         build_decision_node(Features, RightExamples, LeftImportance, TotalLeft, RightError, 
%%                                             ExConf, Conf#rf_tree{depth=Depth + 1}, Id + 2, NoLeftNodes),
%%                     Distribution = {rr_example:count(LeftExamples), rr_example:count(RightExamples), rr_example:majority(Examples)},
%%                     {make_node(Id, Feature, Distribution, Score, LeftNode, RightNode), RightImportance, TotalRight, NoRightNodes+1}
%%             end    
%%     end.

%% @private
laplace(C, N) ->
    (C+1)/(N+2). %% NOTE: no classes?

%% @doc randomly split data set
-spec random_split(#rr_example{}, features(), examples(), distribute_fun(), missing_fun()) -> split().
random_split(ExConf, Feature, Examples, Distribute, Missing) ->
    rr_example:split(ExConf, Feature, Examples, Distribute, Missing).

%% @doc sample a split-value from examples with values for the feature
-spec value_split(#rr_example{}, features(), examples(), distribute_fun(), missing_fun()) -> split().
value_split(ExConf, Feature, Examples, Distribute, Missing) ->
    rr_example:split(ExConf, Feature, Examples, Distribute, Missing,
                     fun (_, {numeric, _FeatureId}, _Ex) ->
                             none; %% TODO: sample from those with value
                         (_, {categoric, _FeatureId}, _Ex) ->
                             none; %% TODO: same..
                         (Me, _Ff, Ex) ->
                             rr_example:sample_split_value(Me, Ex)
                     end).

%% @doc make a determinisc split in the numeric data set
-spec deterministic_split(#rr_example{}, features(), examples(), distribute_fun(), missing_fun()) -> split().
deterministic_split(ExConf, Feature, Examples, Distribute, Missing) ->
    rr_example:split(ExConf, Feature, Examples, Distribute, Missing, 
                     fun (Me, {numeric, FeatureId}, Ex) ->
                             rr_example:find_numeric_split(Me, FeatureId, Ex, fun rr_estimator:info/2);
                         (Me, Ff, Ex) ->
                             rr_example:sample_split_value(Me, Ff, Ex)
                     end).

-ifdef(TEST).

new_tree_test() ->
    Tree = new([]).


-endif.
