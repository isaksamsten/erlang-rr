%%% @author Isak Karlsson <isak@dhcp-159-52.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for generating a proximity matrix from an induced model
%%% @end
%%% Created : 21 Mar 2013 by Isak Karlsson <isak@dhcp-159-52.dsv.su.se>
-module(rr_proximity).
-export([generate_proximity/3]).


generate_proximity(Model, Examples, Conf) ->
    generate_proximity(Model, Examples, Conf, dict:new()).

generate_proximity(_, [], _, Acc) ->
    Acc;
generate_proximity(Model, [{_, _, ExIds}|Rest], Conf, Dict) ->
    NewDict = generate_proximity_for_class(Model, ExIds, Conf, Dict),
    generate_proximity(Model, Rest, NewDict).

generate_proximity_for_class(_, [], Conf, Dict) ->
    Dict;
generate_proximity_for_class(Model, [ExId|Rest], Conf, Dict) ->
    Model ! {evaluate, self(), ExId},
    receive
	{prediction, Model, Predictions} ->
	    NewDict = lists:foldl(fun ({_, _NodeNr}, Acc) ->
					  Acc %% NOTE: use NodeNr to link ExId to similar ExIds
				  end, Dict, Predictions),
	    generate_proximity_for_class(Model, Rest, Conf, NewDict)
    end.
