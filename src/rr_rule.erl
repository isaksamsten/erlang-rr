-module(rr_rule).
-export([add/2,
	 sort/1,
	 new/1,
	 is_empty/1]).

-include("rr.hrl").

add(#rr_rule{antecedent=A} = Rule, Condition) ->
    Rule#rr_rule{antecedent=[Condition|A]}.

sort(#rr_rule{antecedent=A} = Rule) ->
    Rule#rr_rule{antecedent=lists:reverse(A)}.

new(Class) ->
    #rr_rule{consequent=Class, antecedent=[]}.

is_empty(#rr_rule{antecedent=A}) ->
    length(A) == 0.
