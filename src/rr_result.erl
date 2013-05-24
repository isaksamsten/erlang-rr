%%% @author Isak Karlsson <isak@Macintosh.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for writing results
%%% @end
%%% Created : 15 Apr 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_result).
-author('isak-kar@dsv.su.se').
-export([
	 csv/0,
	 default/0
]).

-type result() :: vi | predictions | evaluation | method | start | 'end'.
-type result_fun() :: fun((result(), any()) -> ok).

%% @doc return a csv result generator
-spec csv() -> result_fun().
csv() ->
    fun(Data) ->
	    case rr_config:get_value('output.csv.header', true) of
		true ->
		    Default = ["fold","accuracy", "auc", "oob-base-accuracy", "base-accuracy",
			       "strength", "variance", "correlation", "c/s^2", "brier"],
		    Header = rr_config:get_value('csv.header', Default),
		    io:format("~s~n", [string:join(Header, ",")]);
		false ->
		    ok
	    end,
	    csv_output(Data)
    end.

csv_output({cv, _, Folds}) ->
    csv_output_cv(Folds);
csv_output({split, Split}) ->
    csv_output_split(Split).

csv_output_cv([]) ->
    done;
csv_output_cv([{{_, Fold, _}, Measures}|Rest]) ->
    io:format("fold ~p,", [Fold]),
    csv_output_measures(Measures),
    csv_output_cv(Rest).

csv_output_split({_, Measures}) ->
    csv_output_measures(Measures).

csv_output_measures(Measures) ->
    {accuracy, Accuracy} = lists:keyfind(accuracy, 1, Measures),
    AUC = case lists:keyfind(auc, 1, Measures) of
	      {auc, _, Avg} ->
		  Avg;
	      {auc, Avg} ->
		  Avg
	  end,
    {oob_base_accuracy, Oba} = lists:keyfind(oob_base_accuracy, 1, Measures),
    {base_accuracy, Ba} = lists:keyfind(base_accuracy, 1, Measures),
    {variance, V} = lists:keyfind(variance, 1, Measures),
    {strength, S} = lists:keyfind(strength, 1, Measures),
    {correlation, Corr} = lists:keyfind(correlation, 1, Measures),
    {c_s2, Cs2} = lists:keyfind(c_s2, 1, Measures),
    {brier, Brier} = lists:keyfind(brier, 1, Measures),
    io:format("~p,", [Accuracy]),
    io:format("~p,", [AUC]),
    io:format("~p,", [Oba]),
    io:format("~p,", [Ba]),
    io:format("~p,", [S]),
    io:format("~p,", [V]),
    io:format("~p,", [Corr]),
    io:format("~p,", [Cs2]),
    io:format("~p ~n", [Brier]).

%% @doc return a default result generator (human-readable)
-spec default() -> result_fun().
default() ->
    fun(Data) ->
	    default_output(Data)
    end.

default_output({cv, _, Data}) ->
    OutputFolds = rr_config:get_value('output.cv.folds', false),
    default_output_cv(Data, OutputFolds).

default_output_cv([], _) -> 
    done;
default_output_cv([{{_, Fold, _}, Measures}|Rest], OutputFolds) ->
    if OutputFolds == true ->
	    default_output_measures(Fold, Measures),
	    io:format("~n"),
	    default_output_cv(Rest, OutputFolds);
       OutputFolds == false ->
	    if Fold == average ->
		    default_output_measures(Fold, Measures),
		    default_output_cv(Rest, OutputFolds);
	       true ->
		    default_output_cv(Rest, OutputFolds)
	    end
    end.
		    
default_output_measures(Fold, Measures) ->
    io:format("fold ~p ~n", [Fold]),
    lists:foreach(fun ({accuracy, Accuracy}) ->
			  io:format("accuracy: ~p ~n", [Accuracy]);
		      ({auc, Auc, Avg}) ->
			  io:format("area under ROC~n"),
			  lists:foreach(fun({Class, _, A}) ->
						io:format("  ~s: ~p ~n", [Class, A])
					end, Auc),
			  io:format(" average: ~p ~n", [Avg]);
		      ({oob_accuracy, OOB}) ->
			  io:format("base oob-accuracy: ~p ~n", [OOB]);
		      ({base_accuracy, Base}) ->
			  io:format("base accuracy: ~p ~n", [Base]);
		      ({strength, Margin}) ->
			  io:format("strength: ~p ~n", [Margin]);
		      ({correlation, C}) ->
			  io:format("correlation: ~p ~n", [C]);
		      ({c_s2, Cs2}) ->
			  io:format("c/s^2: ~p ~n", [Cs2]);
		      ({variance, A}) ->
			  io:format("variance: ~p ~n", [A]);
		      ({auc, Auc}) ->
			  io:format("auc: ~p ~n", [Auc]);
		      ({precision, Precision}) ->
			  io:format("precision~n"),
			  lists:foreach(fun({Class, P}) ->
						io:format("  ~s: ~p ~n", [Class, P])
					end, Precision);
		      ({brier, Brier}) ->
			  io:format("brier: ~p ~n", [Brier]);
		      (_) ->
			  ok
		  end, Measures).
    
output_variable_importance([], _, _) ->
    ok;
output_variable_importance([{FeatureId, Score}|Rest], N, No) ->
    if N >= No ->
	    ok;
       true ->
	    io:format("~p: ~p ~n", [rr_example:feature_name(FeatureId), Score]),
	    output_variable_importance(Rest, N + 1, No)
    end.

output_predictions([]) ->
    ok;
output_predictions([{Class, _, ExIds}|Examples]) ->
    output_predictions_for_class(Class, ExIds),
    output_predictions(Examples).

output_predictions_for_class(_, []) ->
    ok;
output_predictions_for_class(Class, [ExId|Rest]) ->
    io:format("~p \t ~p \t", [ExId, Class]),
    Predictions = ets:lookup_element(predictions, ExId, 2),
    {Pred, Prob} = hd(Predictions),
    io:format("~p (~p) ~n", [Pred, Prob]),
    output_predictions_for_class(Class, Rest).
