%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2013 by  <Isak@ISAK-PC>

-module(km).
-compile(export_all).

euclidian(Features, ExA, ExB) ->
%    A = rr_example:vector(ExA),
 %   B = rr_example:vector(ExB),
    calculate_euclidian_distance(Features, ExA, ExB, 0).

calculate_euclidian_distance([], [], [], Acc) ->
    math:sqrt(Acc);
calculate_euclidian_distance([{numeric, _}|Rest], [A|Ar], [B|Br], Acc) ->
    calculate_euclidian_distance(Rest, Ar, Br, Acc + math:pow(A-B, 2));
calculate_euclidian_distance([{categoric, _}|Rest], [A|Ar], [B|Br], Acc) ->
    Value = if A == B -> 1; true -> 0 end,
    calculate_euclidian_distance(Rest, Ar, Br, Acc + math:pow(Value, 2)).
	    

    

