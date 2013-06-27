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

-define(DEFAULT_CSV_HEADERS, [{"fold", fold},
			      {"accuracy", accuracy}, 
			      {"auc", auc}, 
			      {"oob-base-accuracy", oob_base_accuracy},
			      {"base-accuracy", base_accuracy},
			      {"strength", strength}, 
			      {"variance", variance}, 
			      {"correlation", correlation},
			      {"c/s^2", c_s2}, 
			      {"brier", brier}]).


-define(DEFAULT_HEADERS, [{"fold", fold},
			  {"accuracy", accuracy}, 
			  {"auc", auc}, 
			  {"oob-base-accuracy", oob_base_accuracy},
			  {"base-accuracy", base_accuracy},
			  {"strength", strength}, 
			  {"variance", variance}, 
			  {"correlation", correlation},
			  {"c/s^2", c_s2}, 
			  {"brier", brier}]).

-type result() :: vi | predictions | evaluation | method | start | 'end'.
-type result_fun() :: fun((result(), any()) -> ok).

%% @doc return a csv result generator
-spec csv() -> result_fun().
csv() ->
    fun(Data) ->
	    Header = rr_config:get_value('csv.headers', ?DEFAULT_CSV_HEADERS),
	    case rr_config:get_value('output.csv.header', true) of
		true ->
		    io:format("~s~n", [string:join(lists:map(fun ({H, _}) -> H end, Header), ",")]);
		false ->
		    ok
	    end,
	    csv_output(Data, Header)
    end.

csv_output({cv, _, Folds}, Header) ->
    csv_output_cv(Folds, Header);
csv_output({split, Split}, Header) ->
    csv_output_split(Split, Header).

csv_output_cv([], _) ->
    done;
csv_output_cv([{{_, Fold}, Measures}|Rest], Header) ->
    io:format("fold ~p,", [Fold]),
    csv_output_measures(Measures, Header),
    csv_output_cv(Rest, Header).

csv_output_split({_, Measures}, Header) ->
    csv_output_measures(Measures, Header).

csv_output_measures(Measures, Header) ->
    [{_, Last}|NewHeader] = lists:reverse(Header),
    lists:foreach(fun ({_, Key}) ->
			  case lists:keyfind(Key, 1, Measures) of
			      {Key, Value} ->
				  io:format("~p,", [Value]);
			      {Key, _, Value} ->
				  io:format("~p,", [Value]);
			      _ ->
				  ok
			  end
		  end, lists:reverse(NewHeader)),
    case lists:keyfind(Last, 1, Measures) of
	{Last, Value} ->
	    io:format("~p~n", [Value]);
	_ ->
	    rr:illegal("invalid ending in header...")
    end.

%% @doc return a default result generator (human-readable)
-spec default() -> result_fun().
default() ->
    fun(Data) ->
	    Header = rr_config:get_value('default.headers', ?DEFAULT_HEADERS),
	    default_output(Data, Header)
    end.

default_output({cv, _, Data}, Header) ->
    OutputFolds = rr_config:get_value('output.cv.folds', false),
    default_output_cv(Data, OutputFolds, Header).

default_output_cv([], _, _) -> 
    done;
default_output_cv([{{_, Fold}, Measures}|Rest], OutputFolds, Header) ->
    if OutputFolds == true ->
	    default_output_measures(Fold, Measures, Header),
	    io:format("~n"),
	    default_output_cv(Rest, OutputFolds, Header);
       OutputFolds == false ->
	    if Fold == average ->
		    default_output_measures(Fold, Measures, Header),
		    default_output_cv(Rest, OutputFolds, Header);
	       true ->
		    default_output_cv(Rest, OutputFolds, Header)
	    end
    end.
		    
default_output_measures(Fold, Measures, Header) ->
    io:format("fold ~p ~n", [Fold]),
    lists:foreach(fun ({Name, Key}) ->
			  case lists:keyfind(Key, 1, Measures) of
			      {precision, Value} ->
				  io:format("precision~n"),
				  lists:foreach(fun({Class, P}) ->
						io:format("  ~s: ~p ~n", [Class, P])
					end, Value);
			      {Key, Value} ->
				  io:format("~s: ~p~n", [Name, Value]);
			      {Key, Auc, Value} ->
				  io:format("area under ROC~n"),
				  lists:foreach(fun({Class, _, A}) ->
							io:format("  ~s: ~p ~n", [Class, A])
						end, Auc),
				  io:format("average: ~p~n", [Value]);
			      _ ->
				  ok
			  end
		  end, Header).


    %% lists:foreach(fun ({accuracy, Accuracy}) ->
    %% 			  io:format("accuracy: ~p ~n", [Accuracy]);
    %% 		      ({auc, Auc, Avg}) ->
    %% 			  io:format("area under ROC~n"),
    %% 			  lists:foreach(fun({Class, _, A}) ->
    %% 						io:format("  ~s: ~p ~n", [Class, A])
    %% 					end, Auc),
    %% 			  io:format(" average: ~p ~n", [Avg]);
    %% 		      ({oob_accuracy, OOB}) ->
    %% 			  io:format("base oob-accuracy: ~p ~n", [OOB]);
    %% 		      ({base_accuracy, Base}) ->
    %% 			  io:format("base accuracy: ~p ~n", [Base]);
    %% 		      ({strength, Margin}) ->
    %% 			  io:format("strength: ~p ~n", [Margin]);
    %% 		      ({correlation, C}) ->
    %% 			  io:format("correlation: ~p ~n", [C]);
    %% 		      ({c_s2, Cs2}) ->
    %% 			  io:format("c/s^2: ~p ~n", [Cs2]);
    %% 		      ({variance, A}) ->
    %% 			  io:format("variance: ~p ~n", [A]);
    %% 		      ({auc, Auc}) ->
    %% 			  io:format("auc: ~p ~n", [Auc]);
    %% 		      ({precision, Precision}) ->
    %% 			  io:format("precision~n"),
    %% 			  lists:foreach(fun({Class, P}) ->
    %% 						io:format("  ~s: ~p ~n", [Class, P])
    %% 					end, Precision);
    %% 		      ({brier, Brier}) ->
    %% 			  io:format("brier: ~p ~n", [Brier]);
    %% 		      (_) ->
    %% 			  ok
    %% 		  end, Measures).
    
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
