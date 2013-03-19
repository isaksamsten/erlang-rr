-module(rr_proximity).
-compile(export_all).


generate_proximity(Model, Examples) ->
    generate_proximity(Model, Examples, dict:new()).

generate_proximity(_, [], Acc) ->
    Acc;
generate_proximity(Model, [{_, _, ExIds}|Rest], Dict) ->
    NewDict = generate_proximity_for_class(Model, ExIds, Dict),
    generate_proximity(Model, Rest, NewDict).

generate_proximity_for_class(_, [], Dict) ->
    Dict;
generate_proximity_for_class(Model, [ExId|Rest], Dict) ->
    Model ! {evaluate, self(), ExId},
    io:format(standard_error, "Dispatching ~p ~n", [ExId]),
    receive
	{prediction, Model, NodeNr} ->
	    io:format(standard_error, "Received result for ~p ~n", [NodeNr]),
	    NewDict = dict:update(NodeNr, fun(ExIds) ->    
						  dict:update_counter(ExId, 1, ExIds)
					  end, dict:store(ExId, 1, dict:new()), Dict),
	    generate_proximity_for_class(Model, Rest, NewDict)
    end.
