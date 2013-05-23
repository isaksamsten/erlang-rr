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
	  ({base_accuracy, A}) ->
	      io:format("~p,", [A]);
	  ({margin, A}) ->
	      io:format("~p,", [A]);
	  ({precision, _Precision}) ->
	      ok;
	  ({brier, Brier}) ->
	      io:format("~p ~n", [Brier]);
	  (_) -> ok
      end, Measures).

%% @doc return a default result generator (human-readable)
-spec default() -> result_fun().
default() ->
    fun(Data) ->
	    default_output(Data)
    end.

default_output({cv, _, Data}) ->
    OutputFolds = rr_config:get_value('output.folds', false),
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
		      ({margin, Margin}) ->
			  io:format("margin: ~p ~n", [Margin]);
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
