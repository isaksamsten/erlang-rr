-module(rr_heuristic).
-export([new/2,
	 const/3,
	 update/2,
	 pos_c/1,
	 neg_c/1]).
-include("rr.hrl").

new(Evaluator, Classes) ->
    #rr_heuristic{evaluator=Evaluator, classes=Classes}.

apriori(Pos, Neg, H) ->
    H#rr_heuristic{apriori=Pos/(Pos+Neg)}.
    
const(Pos, Neg, H) ->
    H#rr_heuristic{pos = Pos,
		   neg = Neg}.

update(Covered, H) ->
    Pos = rr_example:count('+', Covered),
    Neg = rr_example:count('-', Covered),
    H#rr_heuristic{pos_c=Pos, neg_c=Neg}.

pos_c(H) ->
    H#rr_heuristic.pos_c.

neg_c(H) ->
    H#rr_heuristic.neg_c.
