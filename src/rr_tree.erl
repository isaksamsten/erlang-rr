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
    build_decision_node(Features, Examples, Conf).
    
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
    Prediction = predict(Example, Model, Conf),
    predict_all(Actual, Rest, Model, Conf,
		dict:update(Actual, fun (Predictions) ->
					    [Prediction|Predictions]
				    end, [Prediction], Dict)).

%%
%% Predict what the class for "Attributes"
%%
predict(_, #rr_leaf{class=Class, score=Score}, _) ->
    {Class, Score};
predict(Attributes, #rr_node{feature={{categoric, Id}, SplitValue}, left=Left, right=Right}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    case Value == SplitValue of
	true ->
	    predict(Attributes, Left, Conf);
	false ->
	    predict(Attributes, Right, Conf)
    end;
predict(Attributes, #rr_node{feature={{numeric, Id}, T}, left=Left, right=Right}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    case Value >= T of
	true ->
	    predict(Attributes, Left, Conf);
	false ->
	    predict(Attributes, Right, Conf)
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
build_decision_node([], [], _) ->
    make_leaf([], error);
build_decision_node([], Examples, _) ->
    make_leaf(Examples, rr_example:majority(Examples));
build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, _) ->
    make_leaf(Examples, {Class, Count});
build_decision_node(Features, Examples, #rr_conf{prune=Prune, evaluate=Evaluate, depth=Depth} = Conf) ->
    NoExamples = rr_example:count(Examples),
    case Prune(NoExamples, Depth) of
	true ->
	    make_leaf(Examples, rr_example:majority(Examples));
	false ->
	    case Evaluate(Features, Examples, NoExamples, Conf) of
		no_information ->
		    make_leaf(Examples, rr_example:majority(Examples));
		#rr_candidate{split=[{_, _}]} ->
		    make_leaf(Examples, rr_example:majority(Examples));
		#rr_candidate{feature=Feature, score=Score, split=[{_, LeftExamples}, {_, RightExamples}]}  -> 
		    Left = build_decision_node(Features, LeftExamples, Conf#rr_conf{depth=Depth + 1}),
		    Right = build_decision_node(Features, RightExamples, Conf#rr_conf{depth=Depth + 1}),
		    make_node(Feature, Score, Left, Right)
	    end	   
    end.

%%
%% Create a decision node, on "Feature" scoring "Score",
%% having "Left" and "Right" branches
%%
make_node(Feature, Score, Left, Right) ->
    #rr_node{score=Score, feature=Feature, left=Left, right=Right}.

%%
%% Create a leaf node which predicts "Class"
%%
make_leaf([], Class) ->
    #rr_leaf{score=0, distribution={0, 0}, class=Class};
make_leaf(Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rr_leaf{score=laplace(C, N), distribution={C, N-C}, class=Class}.

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

    Gain = abs(entropy(Examples) - Cand#rr_candidate.score),
    if  Gain =< Delta ->
	    resampled_subset_evaluate_split(ordsets:subtract(Features, ordsets:from_list(Features0)), 
					    Examples, Total, Conf#rr_conf{no_features=NoFeatures - Log}, 
					    NoResamples - 1, Delta, Log);
	true ->
	    Cand
    end.

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
	    evaluate_split(Features0, Examples, Total, Conf).	    
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
random_split(Feature, Examples, _) ->
    rr_example:split(Feature, Examples).

%%
%% Find the best numeric split point deterministically
%%
deterministic_split({numeric, _} = Feature, Examples, #rr_conf{score=Score}) ->
    rr_example:split({Feature, Score}, Examples);
deterministic_split(Feature, Examples, _) ->
    rr_example:split(Feature, Examples).

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
    {_, T, ExSplit} = Split(F, Examples, Conf),
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
	       {_, Threshold, ExSplit} ->
		   #rr_candidate{feature = {F, Threshold}, 
				 score = Score(ExSplit, Total), 
				 split = ExSplit}		       
	   end,
    evaluate_split(Features, Examples, Total, Conf, case Cand#rr_candidate.score < OldScore of
							true -> Cand;
							false -> OldCand
						    end).

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
	


%%
%% Caculate the information for splitting into "ValueSplits"
%%
info(ValueSplits, Total) ->
    info(ValueSplits, Total, 0).

info([], _, Acc) -> Acc;
info([{_Value, Splits}|Rest], Total, Acc) ->
    ClassSum = rr_example:count(Splits),
    info(Rest, Total,
	 Acc + (ClassSum / Total) * entropy(Splits)).

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
