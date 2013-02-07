-module(rr_accuracy).
-include("rr.hrl").
-export([evaluate/1,
	 sort/1,
	 best/1]).

evaluate(#rr_heuristics{pos_c=Pos, neg_c=Neg, pos=P, neg=N}) ->
    (Pos + (N - Neg))/(P+N).

sort(_) ->
    ok.

best(_) ->
    ok.

compare(_, _) ->
    ok.
    
