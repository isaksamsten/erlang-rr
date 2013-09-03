%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for performing cross validation
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(cross_validation).

-export([
	 evaluate/2,
	 average_cross_validation/3
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

%% @doc
%% Perform cross-validation on examples
%% @end
-spec evaluate(example_set(), any()) -> result_set().
evaluate(ExSet, Props) ->
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
    #rr_exset {
       features = Features,
       examples = Examples,
       exconf = ExConf
      } = ExSet,
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
				{H, {List, Auc}} ->
				    case Sum of
					0 ->
					    {average_list_item(List, Folds, 0), Sum + Auc/Folds};
					{ListSum, AvgSum} ->
					    {average_list_item(List, Folds, ListSum), AvgSum + Auc/Folds}
				    end;
				{H, List} when is_list(List) ->
				    average_list_item(List, Folds, Sum);
				{H, O} ->
				    Sum + O/Folds;
				false ->
				    Sum 
			    end
		    end, 0, Avg),
    average_cross_validation(Avg, Folds, Rest, [{H, A}|Acc]).

%% @doc average a list of items
average_list_item(List, Folds, 0) ->
    average_list_item(List, Folds, lists:map(
				     fun ({Class, {_, _}}) -> {Class, {0, 0}};
					 ({Class, _}) -> {Class, 0}
				     end, List));
average_list_item([], _, Acc) -> Acc;
average_list_item([Item|Rest], Folds, Acc) ->
    NewAcc = case Item of
		 {Key, {_, 'n/a'}} -> % note: don't count when we got no value
		     case lists:keytake(Key, 1, Acc) of
			 {value, {Key, {_, B}}, AccRest} ->
			     [{Key, {0, B + 0 / Folds}}|AccRest];
			 false ->
			     [{Key, {0,  1/Folds}}|Acc]
		     end;
		 {Key, {_, A}} ->
		     case lists:keytake(Key, 1, Acc) of
			 {value, {Key, {_, B}}, AccRest} ->
			     [{Key, {0, B + A / Folds}}|AccRest];
			 false ->
			     [{Key, {0,  A/Folds}}|Acc]
		     end;
		 {Key, 'n/a'} ->
		     case lists:keytake(Key, 1, Acc) of
			 {value, {Key, B}, AccRest} ->
			     [{Key, B + 0 / Folds}|AccRest];
			 false ->
			     [{Key, 1/Folds}|Acc]
		     end;
		 {Key, A} ->
		     case lists:keytake(Key, 1, Acc) of
			 {value, {Key, B}, AccRest} ->
			     [{Key, B + A / Folds}|AccRest];
			 false ->
			     [{Key, A/Folds}|Acc]
		     end
	     end,
    average_list_item(Rest, Folds, NewAcc).




