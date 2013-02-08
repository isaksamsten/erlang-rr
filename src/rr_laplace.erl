-module(rr_laplace).
-include("rr.hrl").
-export([evaluate/1,
	 sort/1,
	 best/1,
	 compare/2]).

evaluate(#rr_heuristic{pos_c=Pos, neg_c=Neg}) ->
    (Pos + 1) / (Pos + Neg + 2).

sort(Acc) ->
    lists:sort(fun({_, Ca}, {_, Cb}) ->
		       compare(Ca, Cb)
	       end, Acc).

best(Sorted) ->
    hd(Sorted).

compare(#rr_candidate{score=A}, #rr_candidate{score=B}) ->
    A > B.
