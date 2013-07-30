%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_eval).

-export([
	 cross_validation/4,
	 average_cross_validation/3,
	 split_validation/4,

	 accuracy/1,
	 auc/2,
	 brier/2,
	 precision/1,
	 recall/1,
	 strength/2,
	 variance/2,
	 correlation/4,

	 confusion_matrix/1
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl"). %% note: for specs


-type result_list() :: {atom(), any()} | {atom(), any(), any()}.
-type result() :: {{atom(), atom(), Model::any()}, [result_list(),...]}.
-type result_set() :: {cv, Folds::integer(), [result(),...]} | {split, result()}.

%% @doc
%% Do cross-validation on Examples.
%% @end
-spec cross_validation(features(), examples(), #rr_example{}, any()) -> result_set().
cross_validation(Features, Examples, ExConf, Props) ->
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

    Total0 = rr_example:cross_validation(
	      fun (Train, Test, Fold) ->
		      Progress(Fold),
		      Model = Build(Features, Train, ExConf),
		      Result = Evaluate(Model, Test, ExConf),
		      {{{fold, Fold}, Result}, Model}
	      end, NoFolds, Examples),
    {Total, Models} = lists:unzip(Total0),
    Avg = Average(Total, NoFolds),
    {{cv, NoFolds, Total ++ [Avg]}, Models}.


%% @private default method for averaging the results of cross-validation
average_cross_validation(Result, Folds) ->
    average_cross_validation(Result, Folds, [accuracy, auc, strength, correlation, c_s2, precision, recall,
					     variance, oob_base_accuracy, base_accuracy, brier]).

%% @doc average the cross-validation (for the results specified in Inputs)
average_cross_validation(Result, Folds, Inputs) ->
        average_cross_validation(Result, Folds, Inputs, []).

%% @private average cross-validation
average_cross_validation(_, _, [], Acc) ->
    {{fold, average}, lists:reverse(Acc)};
average_cross_validation(Avg, Folds, [H|Rest], Acc) ->
    A = lists:foldl(fun ({_, Measures}, Sum) ->
			    case lists:keyfind(H, 1, Measures) of
				{H, _, Auc} ->
				    Sum + Auc/Folds;
				{H, List} when is_list(List) ->
				    average_list_item(List, Folds, Sum);
				{H, O} ->
				    Sum + O/Folds;
				false ->
				    Sum 
			    end
		    end, 0, Avg),
    average_cross_validation(Avg, Folds, Rest, [{H, A}|Acc]).

average_list_item(List, Folds, 0) ->
    average_list_item(List, Folds, lists:map(fun ({Class, _}) -> {Class, 0} end, List));
average_list_item(List, Folds, Acc) ->
    lists:zipwith(fun ({Class, A}, {Class, B}) ->
			  {Class, B + A/Folds}
		  end, List, Acc).


%% @doc split examples and train and evaluate
-spec split_validation(features(), examples(), #rr_example{}, any()) -> result_set().
split_validation(Features, Examples, ExConf, Props) ->
    Build = case proplists:get_value(build, Props) of
		undefined -> throw({badarg, build});
		Build0 -> Build0
	    end,
    Evaluate = case proplists:get_value(evaluate, Props) of
		   undefined -> throw({badarg, evaluate});
		   Evaluate0 -> Evaluate0
	       end,

    Ratio = proplists:get_value(ratio, Props, 0.66),
    {Train, Test} = rr_example:split_dataset(Examples, Ratio),
    Model = Build(Features, Train, ExConf),
    Result = Evaluate(Model, Test, ExConf),
    {{split, {{split, Ratio}, Result}}, Model}.
	
%% @doc 
%% Calculate the accuracy (i.e. the percentage of correctly
%% classified examples) 
%% @end
-spec accuracy(dict()) -> Accuracy::float().
accuracy(Predictions) ->
    {Correct, Incorrect} = correct(Predictions),
    Correct / (Correct + Incorrect).

%% @private containing number of {Correct, Incorrect} predictions
correct(Predictions) ->
    dict:fold(fun (Actual, Values, Acc) ->
		      lists:foldl(fun({{Predict, _}, _Probs},  {C, I}) ->
					  case Actual == Predict of
					      true -> {C+1, I};
					      false -> {C, I+1}
					  end
				  end, Acc, Values)
	      end, {0, 0}, Predictions).

%% @doc
%% Calculate the area under ROC for predictions (i.e. the ability of
%% the model to rank true positives ahead of false positives)
%% @end
-spec auc(dict(), integer()) -> [{Class::atom(), NoExamples::integer(), Auc::float()}].
auc(Predictions, NoExamples) ->
    calculate_auc_for_classes(dict:fetch_keys(Predictions), Predictions, NoExamples, []).

calculate_auc_for_classes([], _, _, Acc) ->
    Acc;
calculate_auc_for_classes([Pos|Rest], Predictions, NoExamples, Auc) ->
    PosEx = dict:fetch(Pos, Predictions),
    Sorted = sorted_predictions(
	       lists:map(fun ({_, P}) -> {pos, find_prob(Pos, P)} end, PosEx), 
	       dict:fold(fun(Class, Values, Acc) ->
				 if Class /= Pos ->
					 lists:foldl(fun({_, P}, Acc0) ->
							     [{neg, find_prob(Pos, P)}|Acc0] 
						     end, Acc, Values);
				    true ->
					 Acc
				 end
			 end, [], Predictions)),
    NoPosEx = length(PosEx),
    if NoPosEx > 0 ->
	    calculate_auc_for_classes(Rest, Predictions, NoExamples, 
				      [{Pos, NoPosEx, calculate_auc(Sorted, 0, 0, 0, 0, -1, 
								    NoPosEx, NoExamples - NoPosEx, 0)}|Auc]);
       true ->
	    calculate_auc_for_classes(Rest, Predictions, NoExamples,
				      [{Pos, NoPosEx, 'n/a'}|Auc])
    end.

%% @private calculate auc
calculate_auc([], _Tp, _Fp, Tp_prev, Fp_prev, _Prob_prev, NoPos, NoNeg, Auc) ->
    (Auc + abs(NoNeg - Fp_prev) * (NoPos + Tp_prev)/2)/(NoPos * NoNeg);
calculate_auc([{Class, Prob}|Rest], Tp, Fp, Tp_prev, Fp_prev, OldProb, NoPos, NoNeg, Auc) ->
    {NewAuc, NewProb, NewFp_p, NewTp_p} = if Prob /= OldProb ->
					      {Auc + abs(Fp - Fp_prev) * (Tp + Tp_prev) / 2, Prob, Fp, Tp};
					 true ->
					      {Auc, OldProb, Fp_prev, Tp_prev}
				      end,
    {NewTp, NewFp} = if Class == pos ->
			     {Tp + 1, Fp};
			true ->
			     {Tp, Fp + 1}
		     end,
    calculate_auc(Rest, NewTp, NewFp, NewTp_p, NewFp_p, NewProb, NoPos, NoNeg, NewAuc).
					      
sorted_predictions(Pos, Neg) ->
    lists:sort(fun({_, A}, {_, B}) -> A > B end, Pos ++ Neg).

%% @private Find probability for predicting "Class" in range [0, 1]
find_prob(Class, Probs) ->
    case lists:keyfind(Class, 1, Probs) of
	{Class, Prob} ->
	    Prob;
	false ->
	    0
    end.

%% @doc correlation, calculated as variance()/(1/K)P(h(x)=y)+P(h(x)=j) + (P(h_k(x)=y)-P(h_k(x)=j)) where y /= j
correlation(Predictions, NoExamples, Accuracy, NoTrees) ->
    Nominator = variance(Predictions, NoExamples),
    Denominator = calculate_tree_correlation(Accuracy, 0),
    Nominator/math:pow((1/NoTrees)*Denominator, 2).

calculate_tree_correlation([], Acc) ->
    Acc;
calculate_tree_correlation([{A, B}|Rest], Acc) ->
    calculate_tree_correlation(Rest, Acc + math:sqrt(A + B + math:pow(A - B, 2))).

variance(Predictions, NoExamples) ->
    Strength = strength(Predictions, NoExamples),
    Variance = calculate_value_for_classes(Predictions, fun calculate_variance/3, 0),
    ((1/NoExamples)*Variance) - math:pow(Strength, 2).

calculate_variance([], _, Score) ->
    Score;
calculate_variance([{_, Probs}|Rest], Actual, Score) ->
    NextBest = get_2nd_best_prob(Actual, Probs),
    calculate_variance(Rest, Actual, 
		     case lists:keyfind(Actual, 1, Probs) of
			 {_, Best} ->
			     Score + math:pow(Best - NextBest, 2);
			 false ->
			     Score + math:pow(0 - NextBest, 2)
		     end).  


%% @doc the strength of RF, calculated as: (1/N)(P(h(x) = Y) - P(h(x) = j) where j /= Y)
strength(Predictions, NoExamples) ->
    calculate_value_for_classes(Predictions, fun calculate_strength/3, 0) / NoExamples.

calculate_strength([], _, Score) ->
    Score;
calculate_strength([{_, Probs}|Rest], Actual, Score) ->
    NextBest = get_2nd_best_prob(Actual, Probs),
    calculate_strength(Rest, Actual, 
		     case lists:keyfind(Actual, 1, Probs) of
			 {_, Best} ->
			     Score + Best - NextBest;
			 false ->
			     Score + 0 - NextBest
		     end).    

get_2nd_best_prob(Actual, Probs) ->
    case lists:keydelete(Actual, 1, Probs) of
	[] -> 0;
	[{_, NextBest0}|_] ->
	    NextBest0
    end.

%% @doc
%% Calculate the brier score for predictions (i.e. the mean square
%% difference between the predicted probability assigned to the
%% possible outcomes and the actual outcome)
%% @end
-spec brier(dict(), integer()) -> Brier::float().
brier(Predictions, NoExamples) ->
   calculate_value_for_classes(Predictions, fun calculate_brier_score/3, 0) / NoExamples.

%% @private
calculate_brier_score([], _, Score) ->
    Score;
calculate_brier_score([{_, Probs}|Rest], Actual, Score) ->
    calculate_brier_score(Rest, Actual, lists:foldl(fun ({Class, Prob}, Acc) ->
							    if Class == Actual ->
								    Acc + math:pow(1 - Prob, 2);
							       true ->
								    Acc + math:pow(Prob, 2)
							    end
						    end, Score, Probs)).


calculate_value_for_classes(Predictions, Fun, Score) ->
    calculate_value_for_classes(dict:fetch_keys(Predictions), Predictions, Fun, Score).

%% @private calulate an accumulated score for each estimate
calculate_value_for_classes([], _, _, Score) ->
    Score;
calculate_value_for_classes([Actual|Rest], Predictions, Fun, Score) ->
    calculate_value_for_classes(Rest, Predictions, Fun,
				Fun(dict:fetch(Actual, Predictions), Actual, Score)).



%% @doc Calculate the precision when predicting each class
-spec precision(dict()) -> [{Class::atom(), Precision::float()}].
precision(Matrix) ->
    Classes = dict:fetch_keys(Matrix),
    lists:foldl(fun (Class, Acc) ->
			[{Class, precision_for_class(Class, Matrix)}|Acc]
		end, [], Classes).
    
precision_for_class(Class, Matrix) ->
    Row = dict:fetch(Class, Matrix),
    Tp = dict:fetch(Class, Row),
    Rest = dict:fold(fun (_K, Value, Acc) -> Value + Acc end, 0, Row),
    Tp / Rest.

recall(Matrix) ->
    Classes = dict:fetch_keys(Matrix),
    lists:foldl(fun (Class, Acc) ->
			[{Class, recall_for_class(Class, Matrix)}|Acc]
		end, [], Classes).

recall_for_class(Class, Matrix) ->
    dict:fetch(Class, dict:fetch(Class, Matrix)) /
	dict:fold(fun (_, Dict, Value) ->
			  dict:fetch(Class, Dict) + Value
		  end, 0, Matrix).
			     


confusion_matrix(Predictions) ->
    Classes = dict:fetch_keys(Predictions),
    Dict = lists:foldl(fun (ClassA, Dict) ->
			       lists:foldl(fun (ClassB, Dict2) ->
						   dict:update(ClassA,
							       fun (Dict3) ->
								       dict:store(ClassB, 0, Dict3)
							       end, dict:store(ClassB, 0, dict:new()), Dict2)
					   end, Dict, Classes)
		       end, dict:new(), Classes),
    confusion_matrix(Classes, Predictions, Dict).

confusion_matrix(Classes, Predictions, Acc) ->
    lists:foldl(
      fun (_, Acc0) ->
	      lists:foldl(
		fun (Search, Acc1) ->
			lists:foldl(
			  fun ({{Pred, _}, _}, Acc2) ->
				  dict:update(Search,
					      fun (SearchDict) ->
						      dict:update_counter(Pred, 1, SearchDict)
					      end, Acc2)			      
			  end, Acc1, dict:fetch(Search, Predictions))
		end, Acc0, Classes)
      end, Acc, Classes).
			
    
    

%% for true in classes:
%%   for pred in classes:
%%      for p in predictions[pred]
%%         update[true][pred] += 1
