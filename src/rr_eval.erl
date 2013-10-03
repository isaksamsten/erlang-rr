%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_eval).

-export([
	 accuracy/1,
	 auc/3,
	 brier/2,
	 precision/2,
	 recall/2,
	 strength/2,
	 variance/2,
	 correlation/4,

	 confusion_matrix/1
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

%% -spec cross_validation(example_set(), any()) -> result_set().
%% cross_validation(ExampleSet, Props) ->
%%     cross_validation:evaluate(ExampleSet, Props).

%% %% @doc split examples and train and evaluate
%% -spec split_validation(example_set(), any()) -> result_set().
%% split_validation(ExampleSet, Props) ->
%%     split_validation:evaluate(ExampleSet, Props).
    
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
		      lists:foldl(fun({{Predict, _, _Votes}, _Probs},  {C, I}) ->
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
-spec auc(any(), dict(), integer()) -> [{Class::atom(), NoExamples::integer(), Auc::float()}].
auc(Classes, Predictions, NoExamples) ->
    Auc = calculate_auc_for_classes(Classes, Predictions, NoExamples, []),
    AvgAuc = lists:foldl(fun
			     ({_, {_, 'n/a'}}, Sum) -> 
				 Sum;
			     ({_, {No, A}}, Sum) -> 
				 Sum + No/NoExamples*A
			 end, 0, Auc),
    {auc, Auc, AvgAuc}.

calculate_auc_for_classes([], _, _, Acc) ->
    Acc;
calculate_auc_for_classes([Pos|Rest], Predictions, NoExamples, Auc) ->
    case dict:find(Pos, Predictions) of
	{ok, PosEx} -> 
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
	    true = NoPosEx > 0,
	    calculate_auc_for_classes(Rest, Predictions, NoExamples, 
				      [{Pos, {NoPosEx, calculate_auc(Sorted, 0, 0, 0, 0, -1, 
								     NoPosEx, NoExamples - NoPosEx, 0)}}|Auc]);
	error -> 
	    calculate_auc_for_classes(Rest, Predictions, NoExamples,
				      [{Pos, {0, 'n/a'}}|Auc])
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
	{Class, Prob, _Votes} ->
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
			   {_, Best, _Votes} ->
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
			   {_, Best, _Votes} ->
			       Score + Best - NextBest;
			   false ->
			       Score + 0 - NextBest
		       end).    

get_2nd_best_prob(Actual, Probs) ->
    case lists:keydelete(Actual, 1, Probs) of
	[] -> 0;
	[{_, NextBest0, _Votes}|_] ->
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
    calculate_brier_score(Rest, Actual, lists:foldl(fun ({Class, Prob, _Votes}, Acc) ->
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
-spec precision([atom(),...], dict()) -> [{Class::atom(), Precision::float()}].
precision(Classes, Matrix) ->
    NoClasses = length(Classes),
    Precision = lists:foldl(fun (Class, Acc) ->
				    [{Class, {0, precision_for_class(Class, Matrix)}}|Acc]
			    end, [], Classes),
    AvgPrecision = lists:foldl(fun ({_Class, {_, 'n/a'}}, Acc) ->
				       Acc;
				   ({_Class, {_, Value}}, Acc) ->
				       Acc + Value * 1/NoClasses
			       end, 0, Precision),
    {precision, Precision, AvgPrecision}.

    
precision_for_class(Class, Matrix) ->
    case dict:find(Class, Matrix) of
		    error -> 'n/a';
		    {ok, Row} ->
			Tp = dict:fetch(Class, Row),
			Rest = dict:fold(fun (_K, Value, Acc) -> Value + Acc end, 0, Row),
			Tp / Rest
		end.

recall(Classes, Matrix) ->
    NoClasses = length(Classes),
    Recall = lists:foldl(fun (Class, Acc) ->
				 [{Class, {0, recall_for_class(Class, Matrix)}}|Acc]
			 end, [], Classes),
    AvgRecall = lists:foldl(fun ({_Class, {_, 'n/a'}}, Acc) ->
				    Acc;
				({_Class, {_, Value}}, Acc) ->
				    Acc + Value * 1/NoClasses
			    end, 0, Recall),
    {recall, Recall, AvgRecall}.

recall_for_class(Class, Matrix) ->
    case dict:find(Class, Matrix) of
	error -> 'n/a';
	{ok, Column} ->
	    case dict:find(Class, Column) of
		error -> 'n/a';
		{ok, Value} ->
		    case dict:fold(fun (_, Dict, ValueX) ->
						   ValueB = dict:fetch(Class, Dict),
					   ValueX + ValueB
				   end, 0, Matrix) of
			0 -> 'n/a';
			ValueB ->
			    Value / ValueB
		    end				
	    end
    end.
			     


confusion_matrix(Predictions) ->
    Classes = dict:fetch_keys(Predictions),
    Dict = lists:foldl(
	     fun (ClassA, Dict) ->
		     lists:foldl(
		       fun (ClassB, Dict2) ->
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
			  fun ({{Pred, _Prob, _Votes}, _}, Acc2) ->
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
