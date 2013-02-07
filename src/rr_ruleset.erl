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
-include("rr.hrl").

-compile(export_all).

ruleset(Features, Examples) ->
    {Default, Rest} = default_class(Examples),
    learn_rule_for_class(Features, Rest, 
			 #rr_heuristics{classes=length(Examples)}
			 Examples, []) ++ [{'$default$', Default}].

%%
%% Learn rule for one class at a time
%%
learn_rule_for_class(_, [], _, _, Ruleset) ->
    lists:reverse(Ruleset);
learn_rule_for_class(Features, [Class|Rest], Examples, Heuristics, Ruleset) ->
    Binary = rr_example:to_binary(Class, Examples),
    Pos = rr_example:count('+', Examples),
    Neg = rr_example:count('-', Examples),
    learn_rule_for_class(Features, Rest, Examples, Heuristics,
			 [separate_and_conquer(Features, Class, Binary, 
					       Heuristics#rr_heuristic{pos=Pos,neg=Neg})|Ruleset]).

separate_and_conquer(Features, Class, Examples, Heuristics) ->
    separate_and_conquer(Features, Class, Examples, Heuristics,
			 #rr_rule{consequent=Class,
				  antecedent=[]}).

separate_and_conquer(Features, Class, Examples, Heuristics, Rules) ->
    case learn_one_rule(Features, Examples, Heuristics) of
	{Rule, Features0, Covered} ->
	    Pos = rr_example:count('+', Covered),
	    if  Pos > 0 ->
		    NotCovered = remove_covered(Examples, Covered),
		    separate_and_conquer(Features0, Class, NotCovered, 
					 Heuristics, add_antecedent(Rules, Rule));
		true ->
		    sort_antecedent(Rules)
	    end;
	empty ->
	    sort_antecedent(Rules)
    end.

add_antecedent(#rr_rule{antecedent=A} = Rule, New) ->
    Rule#rr_rule{antecedent=[New|A]}.

sort_antecedent(#rr_rule{antecedent=A}) ->
    Rule#rr_rule{antecedent=lists:reverse(A)}.

remove_covered(Examples, Covered) ->
    lists:map(fun({Class, Count, Ids}) ->
		      case rr_example:get_class(Class, Covered) of
			  {Class, Count0, Ids0} ->
			      {Class, Count - Count0, gb_sets:to_list(gb_sets:subtract(gb_sets:from_list(Ids),
										       gb_sets:from_list(Ids0)))};
			  _ ->
			      {Class, Count, Ids}
		      end
	      end, Examples).
		      
learn_one_rule([], _, _) ->
    empty;
learn_one_rule(Features, Examples, Heuristics) ->
    {{Feature, _} = Rule, NotCovered} = find_best_subspace(Features, Examples),
    {Rule, Features -- [Feature], NotCovered}.

find_best_subspace(Features, Examples) ->
    [{_Accuracy, Rule, Covered}|_] = find_best_subspaces(Features, Examples, []),
    {Rule, Covered}.

find_best_subspaces([], _, Acc) ->
    lists:sort(fun({AccuracyA, _, _}, {AccuracyB, _, _}) ->
		       AccuracyA > AccuracyB
	       end, Acc);
find_best_subspaces([Feature|Features], Examples, Acc) ->
    [{OldValue0, OldCovered0}|RestSplit] = rr_example:split(Feature, Examples),
    BestSplit = lists:foldl(fun({Value, Covered}, {OldAccuracy, {F, OldValue}, OldCovered}) ->
				    Accuracy = accuracy(Covered),
				    case Accuracy > OldAccuracy of
					true ->
					    {Accuracy, {F, Value}, Covered};
					false ->
					    {OldAccuracy, {F, OldValue}, OldCovered}
				    end
			    end, {accuracy(OldCovered0), {Feature, OldValue0}, OldCovered0}, RestSplit),
    find_best_subspaces(Features, Examples, [BestSplit|Acc]).

accuracy([]) ->
    -100000000;
accuracy([{'+', Pos, _}]) ->
    Pos;
accuracy([{'-', Neg, _}]) ->
    0 - Neg;
accuracy([{'+', Pos, _}, {'-', Neg, _}]) ->
    Pos - Neg.
    
%%
%% Returns the {D, C} where D is the default class (with the higest a
%% priori probability) and C is a (ordered) list of classes with the
%% least probable first
%%
default_class(Examples) ->
    [Largest|Rest] = [Class || {Class, _, _} <- lists:reverse(lists:keysort(2, Examples))],
    {Largest, lists:reverse(Rest)}.

evaluate_ruleset(_, Examples) ->
    rr_example:count(Examples) > 10.

test() ->
    rr_example:init(),
    File = csv:reader("../data/mushroom.txt"),
    {Features, Examples} = rr_example:load(File, 4),
    io:format("~p ~n",[Examples]),
    ruleset(Features, Examples).
   
