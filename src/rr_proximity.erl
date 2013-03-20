-module(rr_proximity).
-compile(export_all).


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
