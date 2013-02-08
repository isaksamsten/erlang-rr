-module(rr_rule).
-export([add/3,
	 sort/1,
	 new/1,
	 is_empty/1,
	 score/1]).

-include("rr.hrl").

add(#rr_rule{antecedent=A} = Rule, Condition, Score) ->
    Rule#rr_rule{antecedent=[Condition|A], score=Score}.

sort(#rr_rule{antecedent=A} = Rule) ->
    Rule#rr_rule{antecedent=lists:reverse(A)}.

new(Class) ->
    #rr_rule{consequent=Class, antecedent=[]}.

score(#rr_rule{score=missing}) ->
    -10000000;
score(#rr_rule{score=S}) ->
    S.


is_empty(#rr_rule{antecedent=A}) ->
    length(A) == 0.
