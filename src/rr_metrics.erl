%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%% Evaluation metrics
%%% @end
%%% Created : 17 Jun 2013 by  <isak-kar@dsv.su.se>
-module(rr_metrics).

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

%% @doc correlation, calculated as variance()/(1/K)P(h(x)=y)+P(h(x)=j) + (P(h(x)=y)-P(h(x)=j)) where y /= j
%% @todo this is incorrect?
correlation(Predictions, NoExamples, NoTrees) ->
    Nominator = variance(Predictions, NoExamples),
    Denominator = calculate_value_for_classes(Predictions, fun calculate_correlation/3, 0),
    Nominator/math:pow((1/NoTrees)*Denominator, 2).

calculate_correlation([], _, Score) ->
    Score;
calculate_correlation([{_, Probs}|Rest], Actual, Score) ->
    NextBest = case lists:keydelete(Actual, 1, Probs) of
		   [] -> 0;
		   [{_, NextBest0}|_] ->
		       NextBest0
	       end,
    calculate_correlation(Rest, Actual,
		       case lists:keyfind(Actual, 1, Probs) of
			   {_, Best} ->
			       Score + math:sqrt((Best + NextBest + math:pow(Best - NextBest, 2)));
			   false ->
			       Score + math:sqrt((0 + NextBest + math:pow(0 - NextBest, 2)))
		       end).  


variance(Predictions, NoExamples) ->
    Strength = strength(Predictions, NoExamples),
    Variance = calculate_value_for_classes(Predictions, fun calculate_variance/3, 0),
    ((1/NoExamples)*Variance) - math:pow(Strength, 2).

calculate_variance([], _, Score) ->
    Score;
calculate_variance([{_, Probs}|Rest], Actual, Score) ->
    NextBest = case lists:keydelete(Actual, 1, Probs) of
		   [] -> 0;
		   [{_, NextBest0}|_] ->
		       NextBest0
	       end,
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
    NextBest = case lists:keydelete(Actual, 1, Probs) of
		   [] -> 0;
		   [{_, NextBest0}|_] ->
		       NextBest0
	       end,
    calculate_strength(Rest, Actual, 
		     case lists:keyfind(Actual, 1, Probs) of
			 {_, Best} ->
			     Score + Best - NextBest;
			 false ->
			     Score + 0 - NextBest
		     end).    
    

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
precision(Predictions) ->
    precision_for_classes(dict:fetch_keys(Predictions), Predictions, []).

precision_for_classes([], _, Acc) ->
    Acc;
precision_for_classes([Actual|Rest], Predictions, Acc) ->
    {Tp, Fp} = lists:foldl(fun ({{Pred, _}, _}, {Tp, Fp}) ->
				   if Pred == Actual ->
					   {Tp + 1, Fp};
				      true -> 
					   {Tp, Fp + 1}
				   end
			   end, {0, 0}, dict:fetch(Actual, Predictions)),
    precision_for_classes(Rest, Predictions, [{Actual, Tp / (Tp + Fp)}|Acc]).