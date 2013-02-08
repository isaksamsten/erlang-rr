-module(rr_candidate).
-export([new/3,
	 score/1,
	 coverage/1,
	 value/1]).

-include("rr.hrl").

new(Score, Value, Coverage) ->
    #rr_candidate{score=Score,
		  value=Value,
		  coverage=Coverage}.

score(C) ->
    C#rr_candidate.score.

coverage(C) ->
    C#rr_candidate.coverage.

value(C) ->
    C#rr_candidate.value.
