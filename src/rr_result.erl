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
    lists:foreach(
      fun ({accuracy, Accuracy}) ->
	      io:format("~p,", [Accuracy]);
	  ({auc, _Auc, Avg}) ->
	      io:format("~p,", [Avg]);
	  ({auc, Auc}) ->
	      io:format("~p,", [Auc]);
	  ({oob_accuracy, OOB}) ->
	      io:format("~p,", [OOB]);
	  ({precision, _Precision}) ->
	      ok;
	  ({brier, Brier}) ->
	      io:format("~p ~n", [Brier]); %% NOTE: must be last...
	  (_) -> ok
      end, Measures).

%% @doc return a default result generator (human-readable)
-spec default() -> result_fun().
default() ->
    fun(Data) ->
	    io:format("~p ~n", [Data])
    end.

default_writer(start, Time) ->
    io:format("*** Start (at: ~s) *** ~n", [strftime:f(Time, "%F %T")]);
default_writer('end', Time) ->
    io:format("*** End (at: ~s) *** ~n", [strftime:f(Time, "%F %T")]);
default_writer(vi, {Data, N, No}) ->
    io:format("** Variable Importance ** ~n"),
    output_variable_importance(Data, N, No);
default_writer(predictions, Data) ->
    io:format("** Predictions ** ~n"),
    output_predictions(Data);
default_writer(method, {Method, Data}) ->
    io:format("** ~s ~p ** ~n", [Method, Data]);
default_writer(parameters, Data) ->
    io:format("** Parameters ** ~n"),
    lists:foreach(fun ({file, File}) ->
			  io:format("File: ~p ~n", [File]);
		      ({classifiers, Classifiers}) ->
			  io:format("Trees: ~p ~n", [Classifiers]);
		      ({no_features, NoFeatures}) ->
			  io:format("Features: ~p ~n", [NoFeatures]);
		      ({total_no_features, TotalNoFeatures}) ->
			  io:format("Total No Features: ~p ~n", [TotalNoFeatures]);
		      ({examples, Examples}) ->
			  io:format("Examples: ~p ~n", [Examples]);
		      ({time, Time}) ->
			  io:format("Time: ~p seconds ~n", [Time])
		  end, Data);
default_writer(evaluation, Data) ->
    io:format("** Evaluation ** ~n"),
    lists:foreach(fun ({accuracy, Accuracy}) ->
			  io:format("Accuracy: ~p ~n", [Accuracy]);
		      ({auc, Auc, Avg}) ->
			  io:format("Area under ROC~n"),
			  lists:foreach(fun({Class, _, A}) ->
						io:format("  ~s: ~p ~n", [Class, A])
					end, Auc),
			  io:format(" average: ~p ~n", [Avg]);
		      ({oob_accuracy, OOB}) ->
			  io:format("Base accuracy: ~p ~n", [OOB]);
		      ({auc, Auc}) ->
			  io:format("Auc: ~p ~n", [Auc]);
		      ({precision, Precision}) ->
			  io:format("Precision~n"),
			  lists:foreach(fun({Class, P}) ->
						io:format("  ~s: ~p ~n", [Class, P])
					end, Precision);
		      ({brier, Brier}) ->
			  io:format("Brier: ~p ~n", [Brier]);
		      (_) ->
			  ok
		  end, Data).

    
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
