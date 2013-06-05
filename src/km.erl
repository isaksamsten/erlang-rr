%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2013 by  <Isak@ISAK-PC>

-module(km).
-compile(export_all).

euclidian(Features, ExA, ExB) ->
    calculate_euclidian_distance(Features, ExA, ExB, 0).

calculate_euclidian_distance([], [], [], Acc) ->
    math:sqrt(Acc);
calculate_euclidian_distance([{numeric, Id}|Rest], [A|Ar], [B|Br], Acc) ->
    ValueA = rr_example:feature(Id, A),
    ValueB = rr_example:feature(Id, B),
    calculate_euclidian_distance(Rest, Ar, Br, Acc + math:pow(ValueA-ValueB, 2));
calculate_euclidian_distance([{categoric, Id}|Rest], [A|Ar], [B|Br], Acc) ->
    Value = case rr_example:feature(Id, A) == rr_example:feature(Id, B) of true -> 1; false -> 0 end,
    calculate_euclidian_distance(Rest, Ar, Br, Acc + math:pow(Value, 2)).
	    
average(Features, Examples) ->
    average(Examples, Features, length(Features), []).

average(_, [], _, Acc) ->
    Acc;
average(Examples, [F|Rest], Total, Acc) ->
    Avg = average_value_for_feature(F, Examples, Total, 0),
    average(Examples, Rest, Total, [Avg|Acc]).

average_value_for_feature({Type, _}, [], _, Acc) ->
    case Type of
	numeric -> Acc;
	categori -> ok
    end;
average_value_for_feature({Type, F}, [Ex|Rest], Total, Acc) ->
    case Type of
	numeric ->
	    Value = rr_example:feature(F, Ex) * (1/Total),
	    average_value_for_feature({Type, F}, Rest, Total, Acc+Value);
	categoric ->
	    Value = rr_example:feature(F, Ex),
	    average_value_for_feature({Type, F}, Rest, Total, dict:update_counter(Value, 1, Acc))
    end.
	    
    
