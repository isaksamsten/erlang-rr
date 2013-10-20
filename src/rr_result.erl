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
	 csv/1,
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
			      {"precision", precision},
			      {"recall", recall},
			      {"no-rules", no_rules},
			      {"brier", brier}]).


-define(DEFAULT_HEADERS, [{"accuracy", accuracy}, 
			  {"area under ROC", auc}, 
			  {"oob-base-accuracy", oob_base_accuracy},
			  {"base-accuracy", base_accuracy},
			  {"strength", strength}, 
			  {"variance", variance}, 
			  {"correlation", correlation},
			  {"precision", precision},
			  {"recall", recall},
			  {"no-rules", no_rules},
			  {"c/s^2", c_s2}, 
			  {"brier", brier}]).

-type result() :: vi | predictions | evaluation | method | start | 'end'.
-type result_fun() :: fun((result(), any()) -> ok).

%% @doc return a csv result generator
-spec csv() -> result_fun().
csv(Source) ->
    fun(Data) ->
	    Header = rr_config:get_value('csv.headers', ?DEFAULT_CSV_HEADERS),
	    case rr_config:get_value('output.csv.header', false) of
		true ->
		    io:format("~s~n", [string:join(lists:map(fun ({H, _}) -> H end, Header), ",")]);
		false ->
		    ok
	    end,
	    csv_output(Data, Source, Header)
    end.
csv() ->
    csv(fun (info, Fold) ->
		io:format("fold ~p,", Fold);
	    (value, Value) ->
		io:format("~p,", Value);
	    (value_end, Value) ->
		io:format("~p~n", Value)
	end).

csv_output({cv, _, Folds}, Output, Header) ->
    csv_output_cv(Folds, Output, Header);
csv_output({split, Split}, Output, Header) ->
    csv_output_split(Split, Output, Header).

csv_output_cv([], _, _) ->
    done;
csv_output_cv([{{_, Fold}, Measures}|Rest], Output, Header) ->
    Output(info, [Fold]),
    csv_output_measures(Measures, Output, Header),
    csv_output_cv(Rest, Output, Header).

csv_output_split({_, Measures}, Output, Header) ->
    csv_output_measures(Measures, Output, Header).

csv_output_measures(Measures, Output, Header) ->
    [{_, Last}|NewHeader] = lists:reverse(Header),
    lists:foreach(fun ({_, Key}) ->
			  case lists:keyfind(Key, 1, Measures) of
			      {Key, {_tag, _, Auc}} ->
				  Output(value, [Auc]);
			      {Key, Value} ->
				  Output(value, [Value]);
			      {Key, _, Value} ->
				  Output(value, [Value]);
			      _ ->
				  ok
			  end
		  end, lists:reverse(NewHeader)),
    case lists:keyfind(Last, 1, Measures) of
	{Last, Value} ->
	    Output(value_end, [Value]);
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
    default_output_cv(Data, OutputFolds, Header);
default_output({split, {{ratio, Fraction}, Data}}, Header) ->
    default_output_measures(io_lib:format("test set (~.2f%)", [100*(1-Fraction)]), Data, Header).


default_output_cv([], _, _) -> 
    done;
default_output_cv([{{_, Fold}, Measures}|Rest], OutputFolds, Header) ->
    if OutputFolds == true ->
	    default_output_measures(io_lib:format("fold ~p", [Fold]), Measures, Header),
	    io:format("~n"),
	    default_output_cv(Rest, OutputFolds, Header);
       OutputFolds == false ->
	    if Fold == average ->
		    default_output_measures(io_lib:format("fold ~p", [Fold]), Measures, Header),
		    default_output_cv(Rest, OutputFolds, Header);
	       true ->
		    default_output_cv(Rest, OutputFolds, Header)
	    end
    end.
		    
default_output_measures(Fold, Measures, Header) ->
    io:format("~s ~n", [Fold]),
    lists:foreach(fun ({Name, Key}) ->
			  case lists:keyfind(Key, 1, Measures) of			      
			      {Key, {Type, Value, Auc}} when Type == recall; 
							     Type == precision;
							     Type == auc -> 
				  io:format("~s:~n", [Name]),
				  lists:foreach(fun({Class, {_, P}}) ->
						io:format(" - ~s: ~p ~n", [Class, P])
					end, Value),
				  io:format(" average: ~p~n", [Auc]);			      
			      {Key, Value} ->
				  io:format("~s: ~p~n", [Name, Value]);
			      _ ->
				  rr_log:debug("measure not found ('~p')", [Key]),
				  ok
			  end
		  end, Header).

%% output_variable_importance([], _, _) ->
%%     ok;
%% output_variable_importance([{FeatureId, Score}|Rest], N, No) ->
%%     if N >= No ->
%% 	    ok;
%%        true ->
%% 	    io:format("~p: ~p ~n", [rr_example:feature_name(FeatureId), Score]),
%% 	    output_variable_importance(Rest, N + 1, No)
%%     end.

%% output_predictions([]) ->
%%     ok;
%% output_predictions([{Class, _, ExIds}|Examples]) ->
%%     output_predictions_for_class(Class, ExIds),
%%     output_predictions(Examples).

%% output_predictions_for_class(_, []) ->
%%     ok;
%% output_predictions_for_class(Class, [ExId|Rest]) ->
%%     io:format("~p \t ~p \t", [ExId, Class]),
%%     Predictions = ets:lookup_element(predictions, ExId, 2),
%%     {Pred, Prob} = hd(Predictions),
%%     io:format("~p (~p) ~n", [Pred, Prob]),
%%     output_predictions_for_class(Class, Rest).
