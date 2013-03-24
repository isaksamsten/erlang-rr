%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_tree).
-compile(export_all).

-include("rr_tree.hrl").

%%
%% Stop inducing tree if |Example| < MaxExamples or Depth > MaxDepth
%%
example_depth_stop(MaxExamples, MaxDepth) ->
    fun(Examples, Depth) ->
	    (Examples =< MaxExamples) or (Depth > MaxDepth)
    end.

%%
%% Generate model from Features and Examples
%%
generate_model(Features, Examples, Conf) ->
    Info = info_content(Examples, rr_example:count(Examples)),
    {Tree, Importance, Total, _} = build_decision_node(Features, Examples, Info, dict:new(), 0, Conf, 1),
    io:format("Built tree: ~p ~n", [Total]),
    Tree.
    
%%
%% Evaluate "Examples" using "Model"
%%
evaluate_model(Model, Examples, Conf) ->
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Model, Conf, Acc)
		end, dict:new(), Examples).

%%
%% Predict all "Examples" with the actual class "Actual"
%%
predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    {Prediction, _NodeNr} = predict(Example, Model, Conf, []),
    predict_all(Actual, Rest, Model, Conf,
		dict:update(Actual, fun (Predictions) ->
					    [Prediction|Predictions]
				    end, [Prediction], Dict)).

%%
%% Predict what the class for "Attributes"
%%
predict(_, #rr_leaf{id=NodeNr, class=Class, score=Score}, _Conf, Acc) ->
    {{Class, Score}, [NodeNr|Acc]};
predict(ExId, #rr_node{id=NodeNr, 
		       feature=F, 
		       distribution={LeftExamples, RightExamples, {Majority, Count}},
		       left=Left, 
		       right=Right}, #rr_conf{distribute=Distribute} = Conf, Acc) ->
    NewAcc = [NodeNr|Acc],
    case rr_example:distribute(F, ExId) of
	{'?', _} ->
	    case Distribute(predict, F, ExId, LeftExamples, RightExamples) of
		{left, _} ->
		    predict(ExId, Left, Conf, NewAcc);
		{right, _} ->
		    predict(ExId, Right, Conf, NewAcc);
		ignore ->
		    {{Majority, laplace(Count, LeftExamples+RightExamples)}, NewAcc}
	    end;
	{left, _} ->
	    predict(ExId, Left, Conf, NewAcc);
	{right, _} ->
	    predict(ExId, Right, Conf, NewAcc)
    end.
	    
%% 
%% Build a decision tree node from "Features" and "Examples"
%%  if |Feature| == 0 and |Examples| == 0: make_error_node  
%%  if |Feature| == 0: make_leaf majority(Examples)
%%  if |Classes| == 1: make_leaf Class
%%  else:
%%     if Prune(Examples, Depth): make_leaf majority(Examples)
%%     Split = select_split(Features, Examples)
%%     for S in Split: build_node(Features, S)
%%
%% TODO: count total number of nodes in the tree
%%
build_decision_node([], [], Importance, Info, Total, _, Id) ->
    {make_leaf(Id, [], error), Total, Importance, Info};
build_decision_node([], Examples, Importance, Info, Total, _, Id) ->
    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, Info};
build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, Importance, Info, Total, _, Id) ->
    {make_leaf(Id, Examples, {Class, Count}), Importance, Total, Info};
build_decision_node(Features, Examples, Importance, Info, Total, #rr_conf{prune=Prune, 
									  evaluate=Evaluate, 
									  depth=Depth} = Conf, Id) ->
    NoExamples = rr_example:count(Examples),
    case Prune(NoExamples, Depth) of
	true ->
	    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, Info};
	false ->
	    case Evaluate(Features, Examples, NoExamples, Conf) of
		no_information ->
		    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, 0};
		#rr_candidate{split={_, _}} ->
		    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total, 0};
		#rr_candidate{feature=Feature, 
			      score={Score, LeftError, RightError}, 
			      split={both, LeftExamples, RightExamples}}  ->  
		    {Left, LeftImportance, TotalLeft, LeftInfo} = build_decision_node(Features, LeftExamples, Importance, 
										      Info, Total,
										      Conf#rr_conf{depth=Depth + 1}, Id + 1),
		    {Right, RightImportance, TotalRight, RightInfo} = build_decision_node(Features, RightExamples, LeftImportance, 
											  LeftInfo, TotalLeft,
											  Conf#rr_conf{depth=Depth + 1}, Id + 2),
		    Reduction = RightInfo - (LeftError + RightError),
		    {make_node(Id, Feature, {rr_example:count(LeftExamples), 
					     rr_example:count(RightExamples),
					     rr_example:majority(Examples)}, Score, Left, Right), 
		     dict:update_counter(Feature, Reduction, RightImportance), Total + Reduction, Reduction}
	    end	   
    end.

make_node(Id, Feature, Dist, Score, Left, Right) ->
    #rr_node{id = Id,
	     score=Score, 
	     feature=Feature, 
	     distribution=Dist,
	     left=Left, 
	     right=Right}.

make_leaf(Id, [], Class) ->
    #rr_leaf{id=Id, score=0, distribution={0, 0}, class=Class};
make_leaf(Id, Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rr_leaf{id=Id, score=laplace(C, N), distribution={C, N-C}, class=Class}.

laplace(C, N) ->
    (C+1)/(N+2).


%%
%% Return a functions which resamples log(Features) + 1 k times if arg
%% max gain(Features) < Delta
%%
resampled_evaluate(NoResamples, NoFeatures, Delta) ->
    fun (Features, Examples, Total, Conf) ->
	    resampled_subset_evaluate_split(Features, Examples, Total, Conf, NoResamples, Delta, NoFeatures)
    end.

resampled_subset_evaluate_split(_Features, _Examples, _Total, 
				#rr_conf{no_features=NoFeatures}, _, _, _) when NoFeatures =< 0 ->
    no_information;
resampled_subset_evaluate_split(_Features, _Examples, _Total, _Conf, 0, _, _) ->
    no_information;
resampled_subset_evaluate_split(Features, Examples, Total, 
				#rr_conf{no_features=NoFeatures} = Conf, NoResamples, Delta, Log) ->
    Features0 = if NoFeatures =< Log ->
			Features;
		   true ->
			rr_example:random_features(Features, Log)
		end,

    Cand = evaluate_split(Features0, Examples, Total, Conf),
    Gain = entropy(Examples) - Cand#rr_candidate.score,
    if  Gain =< Delta ->
	    resampled_subset_evaluate_split(ordsets:subtract(Features, ordsets:from_list(Features0)), 
					    Examples, Total, Conf#rr_conf{no_features=NoFeatures - Log}, 
					    NoResamples - 1, Delta, Log);
	true ->
	    Cand
    end.

%%
%% Definitly need another way of determine what constitutes a good
%% feature
%%
weighted_evaluate(NoFeatures, Fraction, NewScores) ->
    fun (_, Examples, Total, Conf) ->
	    weighted_evaluate_split(NewScores, Examples, Total, Conf, NoFeatures, Fraction)
    end.

weighted_evaluate_split({Good, _Bad}, Examples, Total, Conf, NoFeatures, Fraction) ->
    Features0 = rr_example:random_features(Good, NoFeatures),
    evaluate_split(Features0, Examples, Total, Conf).

%%
%% Uses the same algorithm as Weka for resampling non-informative
%% 
weka_evaluate(NoFeatures) ->
    fun(Features, Examples, Total, Conf) ->
	    weka_evaluate_split(Features, Examples, Total, Conf, NoFeatures)
    end.

weka_evaluate_split(_, _, _, #rr_conf{no_features=NoTotal}, _) when NoTotal =< 0 ->
    no_information;
weka_evaluate_split(Features, Examples, Total, #rr_conf{no_features=NoTotal} = Conf, NoFeatures) ->
    Features0 = if NoTotal =< NoFeatures ->
			Features;
		   true -> 
			rr_example:random_features(Features, NoFeatures)
		end,
    Cand = evaluate_split(Features0, Examples, Total, Conf),
    Gain = entropy(Examples) - Cand#rr_candidate.score,
    if Gain =< 0.0 ->
	    weka_evaluate_split(ordsets:subtract(Features, ordsets:from_list(Features0)),
				Examples, Total, Conf#rr_conf{no_features=NoTotal - NoFeatures}, NoFeatures);
       true ->
	    Cand
    end.

%% 
%% Evaluate a subset of "NoFeatures" features
%%
subset_evaluate(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    Features0 = rr_example:random_features(Features, NoFeatures),
	    evaluate_split(Features0, Examples, Total, Conf)
    end.

correlation_evaluate(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    FeaturesA = rr_example:random_features(Features, NoFeatures),
	    FeaturesB = rr_example:random_features(Features, NoFeatures),
	    
	    Combination = lists:zipwith(fun (A, B) -> {combined, A, B} end, FeaturesA, FeaturesB),
	    evaluate_split(Combination, Examples, Total, Conf)
    end.
	    


%%
%% Evalate one randomly selected feature
%%
random_evaluate_split(Features, Examples, Total, #rr_conf{no_features=NoFeatures} = Conf) ->
    Feature = lists:nth(random:uniform(NoFeatures), Features),
    evaluate_split([Feature], Examples, Total, Conf).

%%
%% Evaluate all features to find the best split point
%%
best_evaluate_split(Features, Examples, Total, Conf) ->
    evaluate_split(Features, Examples, Total, Conf).

%%
%% Randomly split Example set on Feature by randomly selecting a
%% threshold (sampled from two examples of different class)
%%
random_split(Feature, Examples, #rr_conf{distribute=Distribute}) ->
    rr_example:split(Feature, Examples, Distribute).

%%
%% Find the best numeric split point deterministically
%%
deterministic_split({numeric, _} = Feature, Examples, #rr_conf{score=Score, distribute=Distribute}) ->
    rr_example:split({Feature, Score}, Examples, Distribute);
deterministic_split(Feature, Examples, #rr_conf{distribute=Distribute}) ->
    rr_example:split(Feature, Examples, Distribute).

%%
%% If random:uniform() =< "Alpha": select the best split
%% deterministically, otherwise select a split randomly
%%
random_split(Alpha) ->
    fun (Feature, Examples, Conf) ->
	    Random = random:uniform(),
	    if Random =< Alpha ->
		    deterministic_split(Feature, Examples, Conf);
	       true ->
		    random_split(Feature, Examples, Conf)
	    end
    end.

%%
%% Evaluate all "Features" to find the "best" according to "Score"
%%
evaluate_split([F|Features], Examples, Total, #rr_conf{score=Score, split=Split} = Conf) ->
    {T, ExSplit} = Split(F, Examples, Conf),
    evaluate_split(Features, Examples, Total, Conf, #rr_candidate{feature={F, T},
								  score=Score(ExSplit, Total),
								  split=ExSplit}).

%%
%% Evaluate a list of candidate split points.
%%
evaluate_split([], _, _, _, Acc) ->
    Acc;
evaluate_split([F|Features], Examples, Total, #rr_conf{score=Score, split=Split} = Conf, 
	       #rr_candidate{score=OldScore} = OldCand) ->
    Cand = case Split(F, Examples, Conf) of
	       {Threshold, ExSplit} ->
		   #rr_candidate{feature = {F, Threshold}, 
				 score = Score(ExSplit, Total), 
				 split = ExSplit}		       
	   end,
    evaluate_split(Features, Examples, Total, Conf, case Cand#rr_candidate.score < OldScore of
							true -> Cand;
							false -> OldCand
						    end).

evaluate_all([], _, _, _, Acc) ->
    lists:keysort(1, Acc);
evaluate_all([Feature|Rest], Examples, Total, #rr_conf{score=Score} = Conf, Acc) ->
    NewAcc = case deterministic_split(Feature, Examples, Conf) of
		 {_, ExSplit} ->
		     [{Score(ExSplit, Total), Feature}|Acc]
	     end,
    evaluate_all(Rest, Examples, Total, Conf, NewAcc).

%%
%% Calculate the gini impurity (except 1- to minimize instead of
%% maximize)
%%
gini(ValueSplits, Total) ->
    gini(ValueSplits, Total, 0).

gini([], _, Acc) -> Acc;
gini([{_Value, Splits}|Rest], Total, Acc) -> 
    Fi = rr_example:count(Splits) / Total,
    gini(Rest, Total, Acc + math:pow(Fi, 2)).
	

info({both, Left, Right}, Total) ->
    LeftInfo = info_content(Left, Total),
    RightInfo = info_content(Right, Total),
    {LeftInfo + RightInfo, LeftInfo, RightInfo};
info({left, Left}, Total) ->
    LeftInfo = info_content(Left, Total),
    {LeftInfo, LeftInfo, 0};
info({right, Right}, Total) ->
    RightInfo = info_content(Right, Total),
    {RightInfo, 0, RightInfo}.

    
info_content(Side, Total) ->
    NoSide = rr_example:count(Side),
    (NoSide / Total) * entropy(Side).
        

%%
%% Calculate the entropy of Examples
%%
entropy(Examples) ->
    Counts = [C || {_, C, _} <- Examples],
    entropy(Counts, lists:sum(Counts)).

entropy(Counts, Total) ->
    -1 * lists:foldl(fun (0, Count) ->
			     Count;
			 (Class, Count) ->
			     Fraction = Class / Total,
			     Count + Fraction * math:log(Fraction)%/math:log(2)
		     end, 0, Counts).
