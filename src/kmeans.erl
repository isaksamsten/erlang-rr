%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2013 by  <Isak@ISAK-PC>

-module(kmeans).
-compile(export_all).
-include("km.hrl").

init() ->
    ets:new(centroids, [named_table, public, set, {read_concurrency, true}]).

euclidian(Features, ExA, Average) ->
    euclidian_distance(Features, ExA, Average, 0).

euclidian_distance([], _, _, Acc) ->
    math:sqrt(Acc);
euclidian_distance([{numeric, Id}|Rest], ExA, ExB, Acc) ->
    ValueA = value(ExA, Id),
    ValueB =  value(ExB, Id),
%    io:format("~p-~p = ~p - ~p ~n", [ExA, ExB, ValueA, ValueB]),
    euclidian_distance(Rest, ExA, ExB, Acc +  math:pow(ValueA - ValueB, 2));
euclidian_distance([{categoric, Id}|Rest], ExA, ExB, Acc) ->
    Pow = case rr_example:feature(ExA, Id) == value(ExB, Id) of true -> 1; false -> 0 end,
    euclidian_distance(Rest, ExA, ExB, Acc + math:pow(Pow, 2)).

average(Features, Examples) ->
    average(Examples, Features, length(Examples), []).
average(_, [], _, Acc) ->
    lists:reverse(Acc);
average(Examples, [F|Rest], Total, Acc) ->
    Avg = average_value_for_feature(F, Examples, Total),
    average(Examples, Rest, Total, [Avg|Acc]).

value({centroid, Ex}, At) ->
    case centroid(Ex, At) of 
	'?' -> 1;
	X -> X
    end;
value(Ex, Id) ->
    case rr_example:feature(Ex, Id) of
	'?' -> 
	    1;
	X ->    
	    X
    end.

average_value_for_feature({Type, _} = X, Examples, Total) ->
    average_value_for_feature(X, Examples, Total,
			      case Type of
				  numeric -> 0;
				  categoric -> dict:new()
			      end).

average_value_for_feature({Type, _}, [], _, Acc) ->
    case Type of
	numeric -> Acc;
	categoric -> element(1, hd(lists:reverse(lists:keysort(2, dict:to_list(Acc)))))
   end;
average_value_for_feature({Type, F}, [Ex|Rest], Total, Acc) ->
    case Type of
	numeric ->
	    Value = case rr_example:feature(Ex, F) of
			'?' -> 0;
			Other -> Other * (1/Total)
		    end,
	    average_value_for_feature({Type, F}, Rest, Total, Acc+Value);
	categoric ->
	    Value = rr_example:feature(Ex, F),
	    average_value_for_feature({Type, F}, Rest, Total, dict:update_counter(Value, 1, Acc))
    end.
	    
kmean(Features, Examples, Conf) ->
    #km{k=K, iterations = Iterations} = Conf,
    Flat = rr_example:flatten(Examples),
    {Rand, _Other} = lists:split(K, rr_example:shuffle(Flat)),
    Centroids = insert_centroids(K, Rand, []),
    kmean(Features, Centroids, Flat, different, Iterations).

kmean(_, Centroids, _, _, 0) ->
    Centroids;
kmean(Features, Centroids, Examples, OldAssign, Iterations) ->
    Assign = assign_examples(Features, Centroids, Examples, dict:new()),
    case has_changed_assignment(Features, Assign, OldAssign) of
	true ->
	    NewCentroids = average_centroids(Features, Assign, []),
	    kmean(Features, NewCentroids, Examples, Assign, Iterations - 1);
	false ->
	    Centroids
    end.

has_changed_assignment(_, _, different) ->
    true;
has_changed_assignment(Features, New, Old) ->
    average_change(Features, New, Old).

average_change(_, [], []) ->
    false;
average_change(_, A, B) when A == [];
			     B == [] ->
    true;
average_change(Features, [{_, ExA}|RestA], [{_, ExB}|RestB]) ->
    case ExA == ExB of
	true ->
	    average_change(Features, RestA, RestB);
	false ->
	    true
    end.

average_centroids(_, [], Acc) ->
    Acc;
average_centroids(Features, [{{centroid, Id}, Close}|Rest], Acc) ->
    case Close of
	[] -> 
	    delete_centroid(Id),	    
	    average_centroids(Features, Rest, Acc);
	_ -> 
	    Avg = average(Features, Close),
	    insert_centroid(list_to_tuple([Id|Avg])),
	    average_centroids(Features, Rest, [{centroid, Id}|Acc])
    end.

assign_examples(_, _, [], Dict) ->
    dict:to_list(Dict);
assign_examples(Features, Centroids, [Ex|Rest], Dict) ->
    Closest = closest(Features, Centroids, Ex),
    assign_examples(Features, Centroids, Rest,
		    dict:update(Closest, fun (Old) ->
						 [Ex|Old]
					 end, [Ex], Dict)).

closest(Features, Centroids, Ex) ->
    {Closest, _} = lists:foldl(fun (Centroid, {Old, Distance}) ->
				       NewDistance = euclidian(Features, Ex, Centroid),
				       if NewDistance < Distance ->
					       {Centroid, NewDistance};
					  true ->
					       {Old, Distance}
				       end
			       end, {undefined, inf}, Centroids),
    Closest.

insert_centroids(_, [], Acc) ->
    Acc;
insert_centroids(N, [Centroid|Rest], Acc) ->
    Vector = rr_example:vector(Centroid),
    insert_centroid(setelement(1, Vector, N)),
    insert_centroids(N - 1, Rest, [{centroid, N}|Acc]).

insert_centroid(Tuple) ->
    ets:insert(centroids, Tuple).

delete_centroid({centroid, Id}) ->
    delete_centroid(Id);
delete_centroid(Id) ->
    ets:delete(centroids, Id).    

centroid({centroid, Id}, At) when is_number(At) ->
    centroid(Id, At);
centroid(Id, At) when is_number(At) ->
    ets:lookup_element(centroids, Id, At + 1).

    
