-module(rr_laplace).
-include("rr.hrl").
-export([evaluate/1,
	 sort/1,
	 best/1,
	 compare/2,
	 stop/2]).

evaluate(#rr_heuristic{pos_c=Pos, neg_c=Neg, classes=Classes}) ->
    (Pos + 1) / (Pos + Neg + Classes).

sort(Acc) ->
    lists:sort(fun({_, Ca}, {_, Cb}) ->
		       compare(Ca, Cb)
	       end, Acc).

best(Sorted) ->
    hd(Sorted).

compare(#rr_candidate{score=A}, #rr_candidate{score=B}) ->
    A > B.

stop(Score, #rr_heuristic{apriori=Apriori}) ->
    Score < Apriori.
