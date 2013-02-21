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
    Prediction = predict(rr_example:example(Example), Model, Conf),
    predict_all(Actual, Rest, Model, Conf,
		dict:update(Actual, fun (Predictions) ->
					    [Prediction|Predictions]
				    end, [Prediction], Dict)).

%%
%% Predict what the class for "Attributes"
%%
predict(_, #rr_leaf{class=Class, score=Score}, _) ->
    {Class, Score};
predict(Attributes, #rr_node{feature={{categoric, Id}, SplitValue}, nodes=Nodes}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    Eq = lists:keyfind('==', 1, Nodes),
    NotEq = lists:keyfind('/=', 1, Nodes),
    case Value == SplitValue of
	true ->
	    predict(Attributes, case Eq of
				    false -> element(2, NotEq);
				    {_, Node} -> Node
				end, Conf);
	false ->
	    predict(Attributes, case NotEq of
				    false -> element(2, Eq);
				    {_, Node} -> Node
				end, Conf)
    end;
predict(Attributes, #rr_node{feature={{numeric, Id}, T}, nodes=Nodes}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    Gt = lists:keyfind('>=', 1, Nodes),
    Lt = lists:keyfind('<', 1, Nodes),
    case Value >= T of
	true ->
	    predict(Attributes, case Gt of
				    false -> element(2, Lt);
				    {_, Node} -> Node
				end, Conf);
	false ->
	    predict(Attributes, case Lt of
				    false -> element(2, Gt);
				    {_, Node} -> Node
				end, Conf)
    end.
	    
%% 
%% TODO: 
%%    * implement support for resampling (if no attribute contains any information),
%%      this could (possibly) be done by implementing a new Evaluate-function 
%%      (resampled_evaluate(Evaluation, Resample))
%%    * make the function tail recursive (for improved performance)
%%    * check rr_example:split(), if that function (ever) returns only
%%       one possible split (otherwise, change the function)
%%       
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
    io:format("NoExamples: ~p Depth: ~p~n", [NoExamples, Depth]),
    case Prune(NoExamples, Depth) of
	true ->
	    make_leaf(Examples, rr_example:majority(Examples));
	false ->
	    case Evaluate(Features, Examples, NoExamples, Conf) of
		0 ->
		    make_leaf(Examples, rr_example:majority(Examples));
		Candidate  -> 
		    case Candidate#rr_candidate.split of
			[{_, ExSplit}] ->
			    io:format("Making leaf out of candidate\n"),
			    make_leaf(Examples, rr_example:majority(Examples));
			_ ->
			    Nodes = build_decision_branches(Features, Candidate, Conf#rr_conf{depth=Depth + 1}),
			    make_node(Candidate, Nodes)
		    end
	    end	   
    end.

%%
%% Build the branches for a candidate split
%%
build_decision_branches(Features, #rr_candidate{split=Split}, Conf) ->
    build_decision_branches(Features, Split, Conf, []).

build_decision_branches(_, [], _, Acc) ->
    Acc;
build_decision_branches(Features, [{Value, Split}|Rest], Conf, Acc) ->
    Node = build_decision_node(Features, Split, Conf),
    build_decision_branches(Features, Rest, Conf, [{Value, Node}|Acc]).

%%
%% Create a decision node
%%
make_node(#rr_candidate{feature=Feature, score=Score}, Nodes) ->
    #rr_node{score=Score, feature=Feature, nodes=Nodes}.

%%
%% Create a leaf node which predicts Class
%%
make_leaf([], Class) ->
    #rr_leaf{score=0, distribution={0, 0}, class=Class};
make_leaf(Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rr_leaf{score=laplace(C, N), distribution={C, N-C}, class=Class}.

%%
%% Calculates the laplace estimate for C and N
%%
laplace(C, N) ->
    (C+1)/(N+2).


resampled_evaluate(NoResamples) ->
    fun (Features, Examples, Total, #rr_conf{no_features=Features}) ->
	    ok
    end.
	    
    

%%
%% Evaluate log2(|Features|) + 1 to find the attribute that splits the
%% dataset best
%%
best_subset_evaluate_split(Features, Examples, Total, #rr_conf{no_features=NoFeatures} = Conf) ->
    Log = round((math:log(NoFeatures) / math:log(2))) + 1,
    Features0 = rr_example:random_features(Features, Log),
    evaluate_split(Features0, Examples, Total, Conf).


%%
%% Evalate one randomly selected feature
%%
random_evaluate_split(Features, Examples, Total, Conf) ->
    Feature = lists:nth(random:uniform(length(Features)), Features),
    evaluate_split([Feature], Examples, Total, Conf).

%%
%% Evaluate all features to find the best split point
%%
best_evaluate_split(Features, Examples, Total, Conf) ->
    evaluate_split(Features, Examples, Total, Conf).


%%
%% Randomly select an evalation method If alpha == 0 only select
%% splits at random, if == 1 only select the log2 best split 
%%
random_evaluator(Alpha) ->
    fun (Features, Examples, Total, Conf) ->
	    Random = random:uniform(),
	    if Random =< Alpha ->
		    random_evaluate_split(Features, Examples, Total, Conf);
	       true ->
		    best_subset_evaluate_split(Features, Examples, Total, Conf)
	    end
    end.

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
%% If random:uniform() =< "Alpha" select the best split
%% deterministically, otherwise select randomly
%%
random_splitter(Alpha) ->
    fun (Feature, Examples, Conf) ->
	    Random = random:uniform(),
	    if Random =< Alpha ->
		    deterministic_split(Feature, Examples, Conf);
	       true ->
		    random_split(Feature, Examples, Conf)
	    end
    end.

evaluate_split([F|Features], Examples, Total, #rr_conf{score=Score, split=Split} = Conf) ->	
    {_, T, ExSplit} = Split(F, Examples, Conf),
    evaluate_split(Features, Examples, Total, Conf, #rr_candidate{feature={F, T},
								  score=Score(ExSplit, Total),
								  split=ExSplit}).

%%
%% Evaluate a list of candidates
%%
evaluate_split([], _, _, _, Acc) ->
    Acc;
evaluate_split([F|Features], Examples, Total, #rr_conf{score=Score, split=Split} = Conf, #rr_candidate{score=OldScore} = OldCand) ->
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
%% Sort a list of candidates
%%
sort_candidates(Acc) ->
    lists:sort(fun(A, B) ->
		       A#rr_candidate.score < B#rr_candidate.score
	       end, Acc).

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

random_score(ValueSplits, Total) ->
    Random = random:uniform(),
    if Random >= 0.5 ->
	    info(ValueSplits, Total);
       true ->
	    gini(ValueSplits, Total)
    end.
	    
       
    

%%
%% Calculate the gini impurity (except 1- to minimize instead of
%% maximize)
%%
gini(ValueSplits, Total) ->
    info(ValueSplits, Total, 0).

gini([], _, Acc) -> Acc;
gini([{_Value, Splits}|Rest], Total, Acc) -> 
    Fi = rr_example:count(Splits) / Total,
    gini(Rest, Total, math:pow(Fi)).
	


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
