%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% Implementaion of an unpruned ruleset
%%%
%%% @end
%%% Created :  5 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_ruleset).
-author('isak-kar@dsv.su.se').
-compile(export_all).

-record(rule, {positive, negative, antecedent, consequent}).
separate_and_conquer(Features, Examples) ->
    {Class, Rest} = order_classes(Examples),
    learn_rule_for_class(Features, Rest, Examples, []) ++ [{'$class', Class}].

%%
%% Learn rule for one class at a time
%%
learn_rule_for_class(_, [], _, Ruleset) ->
    lists:reverse(Ruleset);
learn_rule_for_class(Features, [Class|Rest] = Classes, Examples, Ruleset) ->
    {Rule, Examples0} = learn_one_rule(Class, Features, Examples),
    case evaluate_ruleset(Rule, Examples0) of
	true ->
	    learn_rule_for_class(Features, Classes, Examples0, [Rule|Ruleset]);
	false ->
	    learn_rule_for_class(Features, Rest, Examples0, [Rule|Ruleset])
    end.

learn_one_rule(Class, Features, Examples) ->
    {Rule, Examples0} = find_best_subspace(Class, Features, Examples),
    {{Rule, Class}, Examples0}.

find_best_subspace(Class, [Feature|Rest], Examples) ->
    ok.
    

order_classes(Examples) ->
    [Largest|Rest] = lists:reverse(lists:keysort(2, Examples)),
    {Largest, lists:reverse(Rest)}.

evaluate_ruleset(_, Examples) ->
    rr_example:count(Examples) > 10.

rule_accuracy(Rule, Examples) ->
    {value, {_, [{_, Pos,_}, {_, Neg, _}]}, Rest} = lists:keytake(Rule, 1, Examples),
    Pos-Neg.
    
