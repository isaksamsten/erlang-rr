%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rf_tree).
-author('isak-kar@dsv.su.se').
-export([
	 generate_model/3,
	 evaluate_model/3,

	 predict/4,

	 random_split/4,
	 deterministic_split/4,
	 value_split/4,

	 example_depth_stop/2,

	 info/0,
	 gini/0,
	 gini_info/1,
	 entropy/1
]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @doc prune if to few examples or to deep tree
-spec example_depth_stop(integer(), integer()) -> prune_fun().
example_depth_stop(MaxExamples, MaxDepth) ->
    fun(Examples, Depth) ->
	    (Examples =< MaxExamples) or (Depth > MaxDepth)
    end.

%% @doc generate a decision tree
-spec generate_model(features(), examples(), #rf_tree{}) -> #rf_node{}.
generate_model(Features, Examples, Conf) ->
    Info = info_content(Examples, rr_example:count(Examples)),
    build_decision_node(Features, Examples, dict:new(), 0, Info, Conf, 1).

-spec evaluate_model(#rf_node{}, examples(), #rf_tree{}) -> dict().
evaluate_model(Model, Examples, Conf) ->
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Model, Conf, Acc)
		end, dict:new(), Examples).

%% @private
predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    {Prediction, _NodeNr} = predict(Example, Model, Conf, []),
    predict_all(Actual, Rest, Model, Conf,
		dict:update(Actual, fun (Predictions) ->
					    [{Prediction, 0}|Predictions] 
				    end, [{Prediction, 0}], Dict)). %% note: no other prob (fix?)

%% @doc predict an example according to a decision tree
-spec predict(ExId::exid(), tree(), #rf_tree{}, []) -> prediction().
predict(_, #rf_leaf{id=NodeNr, class=Class, score=Score}, _Conf, Acc) ->
    {{Class, Score}, [NodeNr|Acc]};
predict(ExId, #rf_node{id=NodeNr, 
		       feature=F, 
		       distribution={LeftExamples, RightExamples, {Majority, Count}},
		       left=Left, 
		       right=Right}, #rf_tree{distribute=_Distribute, %% TODO: fix me
					      missing_values=Missing} = Conf, Acc) ->
    NewAcc = [NodeNr|Acc],
    case rr_example:distribute(F, ExId) of
	{'?', _} ->
	    case Missing(predict, F, ExId, LeftExamples, RightExamples) of
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
	    
%% @private induce a decision tree
-spec build_decision_node(Features::features(), Examples::examples(), Importance::dict(), Total::number(), 
			  Error::number(), #rf_tree{}, []) -> {tree(), dict(), number()}.
build_decision_node([], [], Importance, Total, _Error, _, Id) ->
    {make_leaf(Id, [], error), Importance, Total};
build_decision_node([], Examples, Importance, Total, _Error, _, Id) ->
    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total};
build_decision_node(_, [{Class, Count, _ExampleIds}] = Examples, Importance, Total, _Error, _, Id) ->
    {make_leaf(Id, Examples, {Class, Count}), Importance, Total};
build_decision_node(Features, Examples, Importance, Total, Error, #rf_tree{prune=Prune, 
									   branch=Branch, 
									   depth=Depth} = Conf, Id) ->
    NoExamples = rr_example:count(Examples),
    case Prune(NoExamples, Depth) of
	true ->
	    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total};
	false ->
	    case Branch(Features, Examples, NoExamples, Conf) of
		no_information ->
		    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total};
		#rr_candidate{split={_, _}} ->
		    {make_leaf(Id, Examples, rr_example:majority(Examples)), Importance, Total};
		#rr_candidate{feature=Feature, 
			      score={Score, LeftError, RightError}, 
			      split={both, LeftExamples, RightExamples}}  ->  
		    NewReduction = Error - (LeftError + RightError),
		    NewImportance = dict:update_counter(rr_example:feature_id(Feature), NewReduction, Importance),
		    
		    {LeftNode, LeftImportance, TotalLeft} = 
			build_decision_node(Features, LeftExamples, NewImportance, Total + NewReduction, LeftError, 
					    Conf#rf_tree{depth=Depth + 1}, Id + 1),
		    
		    {RightNode, RightImportance, TotalRight} = 
			build_decision_node(Features, RightExamples, LeftImportance, TotalLeft, RightError, 
					    Conf#rf_tree{depth=Depth + 1}, Id + 2),
		    Distribution = {rr_example:count(LeftExamples), rr_example:count(RightExamples), rr_example:majority(Examples)},
		    {make_node(Id, Feature, Distribution, Score, LeftNode, RightNode), RightImportance, TotalRight}
	    end	   
    end.

%% @private create a node
-spec make_node([number(),...], feature(), {number(), number()}, number(), tree(), tree()) -> #rf_node{}.
make_node(Id, Feature, Dist, Score, Left, Right) ->
    #rf_node{id = Id, score=Score, feature=Feature, distribution=Dist, left=Left, right=Right}.

%% @private create a leaf
-spec make_leaf([number(),...], examples(), atom()) -> #rf_leaf{}.
make_leaf(Id, [], Class) ->
    #rf_leaf{id=Id, score=0, distribution={0, 0}, class=Class};
make_leaf(Id, Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rf_leaf{id=Id, score=laplace(C, N), distribution={C, N-C}, class=Class}.

%% @private
laplace(C, N) ->
    (C+1)/(N+2). %% NOTE: no classes?

%% @doc randomly split data set
-spec random_split(features(), examples(), distribute_fun(), missing_fun()) -> split().
random_split(Feature, Examples, Distribute, Missing) ->
    rr_example:split(Feature, Examples, Distribute, Missing).

-spec value_split(features(), examples(), distribute_fun(), missing_fun()) -> split().
value_split(Feature, Examples, Distribute, Missing) ->
    rr_example:split(Feature, Examples, Distribute, Missing,
		     fun ({numeric, _FeatureId}, _Ex) ->
			     none; %% TODO: sample from those with value
			 ({categoric, _FeatureId}, _Ex) ->
			     none; %% TODO: same..
			 (Ff, Ex) ->
			     rr_example:sample_split_value(Ff, Ex)
		     end).


%% @doc make a determinisc split in the numeric data set
-spec deterministic_split(features(), examples(), distribute_fun(), missing_fun()) -> split().
deterministic_split(Feature, Examples, Distribute, Missing) ->
    rr_example:split(Feature, Examples, Distribute, Missing, 
		     fun ({numeric, FeatureId}, Ex) ->
			     rr_example:find_numeric_split(FeatureId, Ex, info());
			 (Ff, Ex) ->
			     rr_example:sample_split_value(Ff, Ex)
		     end).



%% @doc random choice between gini-impurity and entropy
-spec gini_info(float()) -> score_fun().
gini_info(Fraction) ->
    Gini = gini(),
    Info = info(),
    fun (Split, Total) ->
	    Random = random:uniform(),
	    if Random =< Fraction ->
		    Gini(Split, Total);
	       true ->
		    Info(Split, Total)
	    end
    end.


%% @doc return a scoring function for the gini-importance (todo: fix)
-spec gini() -> score_fun().
gini() ->
    fun gini/2.
    
%% @private TODO: fix
gini({both, Left, Right}, Total) ->
    LeftGini = gini_content(Left, Total),
    RightGini = gini_content(Right, Total),
    {1-(LeftGini + RightGini), LeftGini, RightGini};
gini({left, Left}, Total) ->
    LeftGini = gini_content(Left, Total),
    {1-LeftGini, LeftGini, 1.0};
gini({right, Right}, Total) ->
    RightGini = gini_content(Right, Total),
    {1-RightGini, 1.0, RightGini}.
    
gini_content(Examples, _Total) -> 
    Counts = [C || {_, C, _} <- Examples],
    Total = lists:sum(Counts),
    lists:foldl(fun (0.0, Count) ->
			Count;
		    (0, Count) ->
			Count;
		    (Class, Count) ->
			Count + math:pow(Class/Total, 2)
		end, 0.0, Counts).
			

%% @doc return score function for information gain
-spec info() -> score_fun().
info() ->
    fun info/2.

%% @private
info({both, Left, Right}, Total) ->
    LeftInfo = info_content(Left, Total),
    RightInfo = info_content(Right, Total),
    {LeftInfo + RightInfo, LeftInfo, RightInfo};
info({left, Left}, Total) ->
    LeftInfo = info_content(Left, Total),
    {LeftInfo, LeftInfo, 0.0};
info({right, Right}, Total) ->
    RightInfo = info_content(Right, Total),
    {RightInfo, 0.0, RightInfo}.

    
info_content(Side, Total) ->
    NoSide = rr_example:count(Side),
    if NoSide > 0 ->
	    Total * (NoSide / Total) * entropy(Side);
       true ->
	    0.0
    end.
        
%% @doc calculate the entropy
-spec entropy(examples()) -> number().
entropy(Examples) ->
    Counts = [C || {_, C, _} <- Examples],
    entropy(Counts, lists:sum(Counts)).

entropy(Counts, Total) ->
    -1 * lists:foldl(fun (0.0, Count) ->
			     Count;
			 (0, Count) ->
			     Count;
			 (Class, Count) ->
			     Fraction = Class / Total,
			     Count + Fraction * math:log(Fraction)%/math:log(2)
		     end, 0.0, Counts).