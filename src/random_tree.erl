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

         %% prune
         example_depth_stop/2,
         chisquare_prune/1,

         %% standard approaches
         random/0,
         all/0,
         subset/0,
         random_subset/2,

         %% multi-feature approaches
         correlation/0,
         random_correlation/1,
         rule/2,
         random_rule/3,
         choose_rule/2,

         %% example sampling approaches
         sample_examples/3,

         %% resampling approahces
         weka/0,
         resample/2,
         hell/0,

         %% significance approaches
         random_chisquare/1,
         randomly_resquare/2,
         chisquare/1,
         chisquare_decrease/2

        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(random_tree, {
          prune,
          pre_prune,
          depth = 0,
          branch,
          score,
          split,
          distribute,
          missing_values,
          no_features,

          model = undefined
         }).

new(Props) ->
    LogFeatures = fun (T) -> trunc(math:log(T)/math:log(2)) + 1 end,
    NoFeatures = proplists:get_value(no_features, Props, LogFeatures),
    Missing = proplists:get_value(missing_values, Props, fun rf_missing:weighted/6),
    Score = proplists:get_value(score, Props, fun rr_estimator:info_gain/2),
    Prune = proplists:get_value(pre_prune, Props, example_depth_stop(2, 1000)),
    FeatureSampling = proplists:get_value(feature_sampling, Props, fun subset/5),
    Distribute = proplists:get_value(distribute, Props, fun tree:distribute/3),
    Split = proplists:get_value(split, Props, fun tree:random_split/5),
    #random_tree{
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
    {Model, Importance, Sum, NoRules} = build_decision_node(Features, Examples, dict:new(), 0, Info, Dataset, RandomTree, 1, 1),
    {RandomTree#random_tree{model = Model}, Importance, Sum, NoRules}.

evaluate(RandomTree, Dataset) ->
    Model = RandomTree#random_tree.model,
    Examples = dataset:examples(Dataset),
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
                        predict_all(Class, ExampleIds, Model, Dataset, RandomTree, Acc)
                end, dict:new(), Examples).

%% @doc has no effect
kill(_) -> ok.

%% @doc serialize this random tree
serialize(RandomTree) ->
    rr_system:serialize_model(RandomTree).

%% @doc unserialize this random tree
unserialize(Dump) ->
    Dump.

%% @doc prune if to few examples or to deep tree
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

evaluate_model(Model, Examples, Dataset, Config) ->
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
                        predict_all(Class, ExampleIds, Model, Dataset, Config, Acc)
                end, dict:new(), Examples).

%% @private
predict_all(_, [], _, _ExConf, _Conf, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Dataset, Config, Dict) ->
    {Prediction, _NodeNr} = predict(Example, Model, Dataset, Config, []),
    predict_all(Actual, Rest, Model, Dataset, Config,
                dict:update(Actual, fun (Predictions) ->
                                            [{Prediction, 0}|Predictions]
                                    end, [{Prediction, 0}], Dict)). %% note: no other prob (fix?)

%% @doc predict an example according to a decision tree
predict(_, #rf_leaf{id=NodeNr, class=Class, score=Score}, _ExConf, _Conf, Acc) ->
    {{Class, Score, []}, [NodeNr|Acc]};
predict(ExId, Node, Dataset, Config, Acc) ->
    #rf_node {
       id=NodeNr,
       feature=F,
       distribution={LeftExamples, RightExamples, {Majority, Count}},
       left=Left,
       right=Right} = Node,
    #random_tree{distribute=Distribute, missing_values=Missing} = Config,
    NewAcc = [NodeNr|Acc],
    case Distribute(Dataset, F, ExId) of
        {'?', _} ->
            case Missing(predict, Dataset, F, ExId, LeftExamples, RightExamples) of
                {left, _} ->
                    predict(ExId, Left, Dataset, Config, NewAcc);
                {right, _} ->
                    predict(ExId, Right, Dataset, Config, NewAcc);
                ignore ->
                    {{Majority, laplace(Count, LeftExamples+RightExamples)}, NewAcc}
            end;
        {left, _} ->
            predict(ExId, Left, Dataset, Config, NewAcc);
        {right, _} ->
            predict(ExId, Right, Dataset, Config, NewAcc)
    end.

%% @private create a node
make_node(Id, Feature, Dist, Score, Left, Right) ->
    #rf_node{id = Id, score=Score, feature=Feature, distribution=Dist, left=Left, right=Right}.

%% @private create a leaf
make_leaf(Id, [], Class) ->
    #rf_leaf{id=Id, score=0, distribution={0, 0}, class=Class};
make_leaf(Id, Covered, {Class, C}) ->
    N = tree:count(Covered),
    #rf_leaf{id=Id, score=laplace(C, N), distribution={C, N-C}, class=Class}.

%% @private induce a decision tree
build_decision_node([], [], Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, [], error), Importance, Total, NoNodes};
build_decision_node([], Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, Examples, tree:majority(Examples)), Importance, Total, NoNodes};
build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, Importance, Total, _Error, _ExConf, _Conf, Id, NoNodes) ->
    {make_leaf(Id, Examples, {Class, Count}), Importance, Total, NoNodes};
build_decision_node(Features, Examples, Importance, Total, Error, ExConf, Conf, Id, NoNodes) ->
    #random_tree{prune=Prune, pre_prune = _PrePrune, branch=Branch, depth=Depth} = Conf,
    NoExamples = tree:count(Examples),
    case Prune(NoExamples, Depth) of
        true ->
            {make_leaf(Id, Examples, tree:majority(Examples)), Importance, Total, NoNodes};
        false ->
            case rf_branch:unpack(Branch(Features, Examples, NoExamples, ExConf, Conf)) of
                no_information ->
                    {make_leaf(Id, Examples, tree:majority(Examples)), Importance, Total, NoNodes};
                #candidate{split={_, _}} ->
                    {make_leaf(Id, Examples, tree:majority(Examples)), Importance, Total, NoNodes};
                #candidate{feature={Feature, _} = FeatureValue,
                           score={Score, LeftError, RightError},
                           split={both, LeftExamples, RightExamples}}  ->
                    NewReduction = Error - (LeftError + RightError),
                    NewImportance = dict:update_counter(feature:id(Feature), NewReduction, Importance),

                    {LeftNode, LeftImportance, TotalLeft, NoLeftNodes} =
                        build_decision_node(Features, LeftExamples, NewImportance, Total + NewReduction, LeftError,
                                            ExConf, Conf#random_tree{depth=Depth + 1}, Id + 1, NoNodes),

                    {RightNode, RightImportance, TotalRight, NoRightNodes} =
                        build_decision_node(Features, RightExamples, LeftImportance, TotalLeft, RightError,
                                            ExConf, Conf#random_tree{depth=Depth + 1}, Id + 2, NoLeftNodes),
                    Distribution = {tree:count(LeftExamples), tree:count(RightExamples), tree:majority(Examples)},
                    {make_node(Id, FeatureValue, Distribution, Score, LeftNode, RightNode), RightImportance, TotalRight, NoRightNodes+1}
            end
    end.

subset() ->
    fun subset/5.

correlation() ->
    fun correlation/5.

random() ->
    fun random/5.

all() ->
    fun all/5.


%% @doc evaluate a subset of n random features
subset(Features, Examples, Total, ExConf, Conf) ->
    #random_tree {
       score = Score,
       split = Split,
       distribute = Distribute,
       missing_values = Missing,
       no_features = NoFeatures
      } = Conf,
    NewFeatures = rr_example:random_features(Features, NoFeatures(length(Features))),
    {tree:best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing), NewFeatures}.

%% @doc evaluate the combination of (n*n)-1 features
correlation(Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score,
       split = Split,
       distribute = Distribute,
       missing_values = Missing,
       no_features = NoFeatures
      } = Conf,
    Select = NoFeatures(length(Features)),
    FeaturesA = rr_example:random_features(Features, Select),
    FeaturesB = rr_example:random_features(Features, Select),

    Combination = [{combined, A, B} || A <- FeaturesA, B <- FeaturesB, A =/= B],
    rr_example:best_split(ExConf, Combination, Examples, Total,
                          Score, Split, Distribute, Missing).

%% @doc tandomly pick either a subset brancher or a correlation brancher
random_correlation(Fraction) ->
    Corr = correlation(),
    Sub = subset(),
    random(Corr, Sub, Fraction).

%% @doc evalate one randomly selected feature (maximum diversity)
random (Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score,
       split = Split,
       distribute = Distribute,
       missing_values = Missing
      } = Conf,
    NoFeatures = length(Features),
    Feature = [lists:nth(random:uniform(NoFeatures), Features)],
    rr_example:best_split(ExConf, Feature, Examples, Total,
                          Score, Split, Distribute, Missing).

%% @doc evaluate all features to find the best split point
all (Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score,
       split = Split,
       distribute = Distribute,
       missing_values = Missing
      } = Conf,
    rr_example:best_split(ExConf, Features, Examples, Total,
                          Score, Split, Distribute, Missing).

%% @doc generate a rule at each branch
rule(NoRules, RuleScore) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            NewConf = Conf#rf_tree {
                        split = fun rf_tree:deterministic_split/5
                       },
            #rf_tree{
               no_features = NoFeatures
              } = Conf,
            FeatureCount = length(Features),
            rf_rule:best(Features, Examples, Total, ExConf, NewConf,
                         NoFeatures(FeatureCount), NoRules(FeatureCount), RuleScore)
    end.

%% @doc randomly pick a subset-brancher or a rule-bracher at each node
random_rule(NoRules, RuleScore, Prob) ->
    Rule = rule(NoRules, RuleScore),
    Sub = subset(),
    random(Rule, Sub, Prob).

%% @doc choose either rule or subset depending on which is best
choose_rule(NoRules, RuleScore) ->
    Rule = rule(NoRules, RuleScore),
    Sub = subset(),
    n([Rule, Sub], fun sig/3).

%% @doc choose the candidate which is most significant
sig([#candidate{split = As}=A, #candidate{split=Bs} = B], Examples, Total) ->
    Achi = rr_estimator:chisquare(As, Examples, Total),
    Bchi = rr_estimator:chisquare(Bs, Examples, Total),
    if Achi > Bchi ->
            A;
       true ->
            B
    end.

%% @doc sample a set of examples of Size, if we can
%% find a significant split use it o/w retry @end
sample_examples(NoFeatures, Size, Sigma) ->
    Do = sample_examples(NoFeatures, Size),
    Redo = sample_examples_significance(NoFeatures, Size, Sigma),
    redo(Do, Redo).

sample_examples(_NoFeatures, Size) when Size >= 1.0 ->
    fun (_, _, _, _) ->
            {Size, {invalid_subset, []}}
    end;
sample_examples(NoFeatures, Size) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            #rf_tree{
               score = Score,
               split = Split,
               distribute = Distribute,
               missing_values = Missing
              } = Conf,
            FeatureSubset = rr_example:random_features(Features, NoFeatures),
            Subset = sample_sane_examples(Examples, Size, Size, Total),
            #candidate{feature=F} =
                rr_example:best_split(ExConf, FeatureSubset, Subset,
                                      rr_example:count(Subset), Score, Split, Distribute, Missing),
            ExSplit = rr_example:split_feature_value(ExConf, F, Examples, Distribute, Missing),
            Best = #candidate{feature = F,
                              split = ExSplit,
                              score = Score(ExSplit, rr_example:count(Examples))},
            {Size, {Best, []}}
    end.

sample_sane_examples(Examples, Size, _,  _) when Size >= 1 ->
    Examples;
sample_sane_examples(Examples, Size, Step, Total) ->
    if Total * Size < 2 ->
            Examples;
       true ->
            case rr_example:subset(Examples, Size) of
                Subset when length(Subset) > 1 -> Subset;
                _ ->
                    sample_sane_examples(Examples, Size+Step, Step, Total)
            end
    end.

sample_examples_significance(NoFeatures, Size, Sigma) ->
    fun (#candidate{split=Split}, NewSize, Examples, Total) ->
            S = rr_estimator:chisquare(Split, Examples, Total),
            if S < Sigma ->
                    Inc = NewSize + Size,
                    {true, sample_examples(NoFeatures, Inc), sample_examples_significance(NoFeatures, Inc, Sigma)};
               true ->
                    false
            end;
        (invalid_subset, _, _, _) ->
            no_information
    end.

%% @doc subset with a slight variance in number of sampled features
random_subset(NoFeatures, Variance) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            random_subset(Features, Examples, Total, ExConf, Conf, NoFeatures, Variance)
    end.

random_subset(Features, Examples, Total, ExConf, Conf, NoFeatures, Variance) ->
    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
    Random0 = random:uniform(),
    Random1 = random:uniform(),

    NewVariance = Random0 * Variance * if Random1 > 0.5 -> 1; true -> 1 end,
    NewNoFeatures = case round(NoFeatures + NewVariance) of
                        X when X > length(Features) -> length(Features);
                        X when X < 1 -> 1;
                        X -> X
                    end,
    NewFeatures = rr_example:random_features(Features, NewNoFeatures),
    rr_example:best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing).


%% @doc
%% resample a decreasing number of features each time an
%% insignificant feature is found, until only one feature is inspected
%% @end
chisquare_decrease(Rate, Sigma) ->
    Sample = redo_curry(subset()),
    redo(Sample, chisquare_decrease_resample(Sample, Rate, Sigma)).

chisquare_decrease_resample(_Sample, Rate, Sigma) ->
    fun (#candidate{split=Split}, NoFeatures, Examples, Total) ->
            Significance = rr_estimator:chisquare(Split, Examples, Total),
            if Significance < Sigma ->
                    NewNoFeatures = if NoFeatures > 1 ->
                                            fun (_) -> round(NoFeatures * Rate) end;
                                       true ->
                                            fun (_) -> 1 end
                                    end,
                    NewSample = redo_curry(NewNoFeatures, subset()),
                    {true, NewSample, chisquare_decrease_resample(NewSample, Rate, Sigma)};
               true ->
                    false
            end
    end.

%% @doc randomly select either chisquare or subset
random_chisquare(Sigma) ->
    Chi = chisquare(Sigma),
    Sub = subset(),
    random(Chi, Sub, 0.5).

%% @doc resample features if chi-square significance is lower than Sigma
chisquare(Sigma) ->
    Sample = redo_curry(subset()),
    Resample = chisquare_resample(Sigma),
    redo(Sample, Resample).

chisquare_resample(Sigma) ->
    fun (#candidate{split=Split}, _, Examples, Total) ->
            Significance = rr_estimator:chisquare(Split, Examples, Total),
            if Significance < Sigma ->
                    {true, redo_curry(subset()), chisquare_resample(Sigma)};
               true ->
                    false
            end
    end.

%% @doc rame as chisquare-resample, however the resampling are done randomly
randomly_resquare(Factor, Sigma) ->
    Sample = redo_curry(subset()),
    Resample = randomly_resquare_redo(Factor, Sigma),
    redo(Sample, Resample).

randomly_resquare_redo(Factor, Sigma) ->
    fun (#candidate{split=Split}, _, Examples, Total) ->
            Significance = rr_estimator:chisquare(Split, Examples, Total),
            Random = random:uniform(),
            if Significance < Sigma, Random < Factor ->
                    {true, redo_curry(subset()), randomly_resquare_redo(Factor, Sigma)};
               true ->
                    false
            end
    end.

%% @doc resample n features m times if gain delta
resample(NoResamples, Delta) ->
    Sample = redo_curry(subset()),
    Resample = simple_resample(Sample, NoResamples, Delta),
    redo(Sample, Resample).

simple_resample(_, 0, _) ->
    fun (_, _, _, _) -> false end;
simple_resample(Sample, NoResamples, Delta) ->
    fun (#candidate{score = {Score, _, _}}, _, Examples, Total) ->
            Gain = (Total * rr_estimator:entropy(Examples)) - Score,
            if Gain =< Delta ->
                    {true, Sample, simple_resample(Sample, NoResamples - 1, Delta)};
               true ->
                    false
            end
    end.

hell() ->
    Sample = redo_curry(subset()),
    Resample = hell_resample(),
    redo(Sample, Resample).

hell_resample() ->
    fun(#candidate{score = {Score, _, _}}, _, _, _) ->
            if Score >= 1.0 ->
                    NewSample = redo_curry(fun(_) -> 1 end, subset()),
                    {true, NewSample, hell_resample()};
               true ->
                    false
            end
    end.


%% @doc resample 1 feature if no informative features is found
weka() ->
    Sample = redo_curry(subset()),
    Resample = weka_resample(),
    redo(Sample, Resample).

weka_resample() ->
    fun (#candidate{score = {Score, _, _}}, _, Examples, Total) ->
            Gain = (Total * rr_estimator:entropy(Examples)) - Score,
            if Gain =< 0.0 ->
                    NewSample = redo_curry(fun (_) -> 1 end, subset()),
                    {true, NewSample, weka_resample()};
               true ->
                    false
            end
    end.

%% @doc
%% Generic function for performing resampling. The first argument - Sample - is used for sampling
%% features and for finding a candidate. The function should return a tuple with the number of sampled
%% features, and {BestCandidate, SampledFeatures}. The second argument - resample - should be used for
%% determining if resampling is needed. This function returns {true, NewSample, NewResample} if
%% no informative feature is found, o/w false.
%% @end
redo(Do, Redo) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            redo(Features, Examples, Total, ExConf, Conf,
                 length(Features), #candidate{score = {inf}}, Do, Redo)
    end.

redo(_, _, _, _, _, 0, _, _, _) ->
    no_information;
redo(Features, Examples, Total, ExConf, Conf, TotalNoFeatures, _Prev, Do, Redo) ->
    {F, {Best, NewFeatures}} = Do(Features, Examples, Total, ExConf, Conf),
    NoFeatures = F(TotalNoFeatures),
    case Redo(Best, NoFeatures, Examples, Total) of
        {true, NewDo, NewRedo} ->
            redo(ordsets:subtract(Features, ordsets:from_list(NewFeatures)),
                 Examples, Total, ExConf, Conf,
                 if is_integer(NoFeatures) ->
                         TotalNoFeatures - NoFeatures;
                    true ->
                         TotalNoFeatures
                 end, Best, NewDo, NewRedo);
        false ->
            {Best, NewFeatures};
        no_information ->
            no_information
    end.

%% @doc call either One or Two randomly according to T
random(One, Two, T) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            R = random:uniform(),
            if R =< T ->
                    One(Features, Examples, Total, ExConf, Conf);
               true ->
                    Two(Features, Examples, Total, ExConf, Conf)
            end
    end.

%% @doc apply n Feature selection algorithms and use Choose to pick one
n([],_, Examples, Total, _ExConf, _Conf, Choose, Acc) ->
    Choose(Acc, Examples, Total);
n([Fun|Rest], Features, Examples, Total, ExConf, Conf, Choose, Acc) ->
    n(Rest, Features, Examples, Total, ExConf, Conf, Choose,
      [unpack(Fun(Features, Examples, Total, ExConf, Conf))|Acc]).

%% @doc apply Funs then Choose one
n(Funs, Choose) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            n(Funs, Features, Examples, Total, ExConf,Conf, Choose, [])
    end.


%% @doc wrap sample-fun Fun to return the number of sampled features
redo_curry(NoFeatures, Fun) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            {NoFeatures, Fun(Features, Examples, Total,
                             ExConf, Conf#rf_tree{no_features=NoFeatures})}
    end.

redo_curry(Fun) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
            {Conf#rf_tree.no_features, Fun(Features, Examples, Total,
                                           ExConf, Conf)}
    end.

%% @doc unpack a candidate
unpack({Candidate, _Features}) ->
    Candidate;
unpack(Candidate) ->
    Candidate.

%% @private
laplace(C, N) ->
    (C+1)/(N+2). %% NOTE: no classes?

-ifdef(TEST).

new_tree_test() ->
    Tree = new([]),
    Dataset = classification_dataset:load(csv:binary_reader("../data/iris.txt")),
    {NewTree, _, _, _} = classifier:build(Tree, Dataset),
    Predictions = classifier:evaluate(NewTree, Dataset),
    ?assertEqual(true, is_list(dict:to_list(Predictions))),
    ?assertEqual(true, is_record(NewTree, random_tree)).

-endif.
