-module(rr_accuracy).
-include("rr.hrl").
-export([evaluate/1,
	 sort/1,
	 best/1,
	 compare/2,
	 stop/2]).

evaluate(#rr_heuristic{pos_c=Pos, neg_c=Neg, pos=P, neg=N} = H) ->
    (Pos + (N - Neg))/(P+N).

sort(Acc) ->
    lists:sort(fun({_, Ca}, {_, Cb}) ->
		       compare(Ca, Cb)
	       end, Acc).

best(Sorted) ->
    hd(Sorted).

compare(#rr_candidate{score=A}, #rr_candidate{score=B}) ->
    A > B.

stop(Score, #rr_heuristic{apriori=A}) ->
    Score < 0.5.
