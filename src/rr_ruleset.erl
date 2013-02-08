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
    H = rr_heuristic:new(rr_purity, length(Examples)),
    Ruleset = learn_rules_for_class(Features, Rest, Examples, H, []),
    Ruleset ++ [{'$default$', Default}].

%%
%% Learn rule for one class at a time
%%
learn_rules_for_class(_, [], _, _, Ruleset) ->
    lists:reverse(Ruleset);
learn_rules_for_class(Features, [Class|Rest], Examples, Heuristics, Ruleset) ->
    Binary = rr_example:to_binary(Class, Examples),
    Pos = rr_example:count('+', Binary),
    Neg = rr_example:count('-', Binary),
    Heu = rr_heuristic:const(Pos, Neg, Heuristics),


    Rule = learn_rule_for_class(Features, Class, Binary, Heu, []),
    learn_rules_for_class(Features, Rest, Examples, Heuristics, [Rule|Ruleset]).

learn_rule_for_class(Features, Class, Examples, Heuristics, ClassRules) ->
    {Rule, NotCovered} = separate_and_conquer(Features, Class, Examples, Heuristics),

    Pos = rr_example:count('+', NotCovered),
    Neg = rr_example:count('-', NotCovered),
    io:format("learn_rule_for_class(~p): ~p/~p pos/neg left\n", [Class, Pos, Neg]),

    case rr_rule:is_empty(Rule) of %% Note: there were no good rules
	false -> 
	    learn_rule_for_class(Features, Class, NotCovered, Heuristics, [Rule|ClassRules]);
	true ->  
	    ClassRules
    end.

	

separate_and_conquer(Features, Class, Examples, Heuristics) ->
    separate_and_conquer(Features, Class, Examples, Heuristics, rr_rule:new(Class)).

separate_and_conquer([], _, NotCovered, _, Rules) ->
    {rr_rule:sort(Rules), NotCovered};    
separate_and_conquer(Features, Class, Examples, 
		     #rr_heuristic{evaluator=Evaluator} = Heuristics, Rules) ->
    {Score, {Feature, _} = Condition, NotCovered} = learn_one_rule(Features, Examples, Heuristics),
    Rules0 = rr_rule:add(Rules, Condition),
    io:format("Rule ~p Scored: ~p\n", [Rules0, Score]),
    case Evaluator:stop(Score, Heuristics) of
	false ->
	    separate_and_conquer(Features -- [Feature], Class, NotCovered, Heuristics, Rules0);
	true ->
	    {rr_rule:sort(Rules), NotCovered}
    end.

remove_covered(Examples, Covered) ->
    lists:map(fun({Class, Count, Ids}) ->
		      case rr_example:get_class(Class, Covered) of
			  {Class, Count0, Ids0} ->
			      NewIds = gb_sets:to_list(gb_sets:subtract(gb_sets:from_list(Ids),
									gb_sets:from_list(Ids0))),
			      {Class, Count - Count0, NewIds};
			  _ ->
			      {Class, Count, Ids}
		      end
	      end, Examples).

%%
%% Learn the best possible Rule from Features and Examples
%% 		      
learn_one_rule(Features, Examples, Heuristics) ->
    {Feature, Candidate} = find_best_subspace(Features, Examples, Heuristics),
    Covered = rr_candidate:coverage(Candidate),
    Value = rr_candidate:value(Candidate),
    Score = rr_candidate:score(Candidate),
    {Score, {Feature, Value}, remove_covered(Examples, Covered)}.

%%
%% Find the best subspace to cover (i.e. what feature is the best)
%%
find_best_subspace(Features, Examples, Heuristics) ->
    [{Feature, Covered}|_] = find_best_subspaces(Features, Examples, Heuristics, []),
    {Feature, Covered}.

find_best_subspaces([], _, #rr_heuristic{evaluator=E}, Acc) ->
    E:sort(Acc);   
find_best_subspaces([Feature|Features], Examples, Heuristics, Acc) ->
    Split = rr_example:split(Feature, Examples),
    BestValue = best_split_value(Heuristics, Split),
    find_best_subspaces(Features, Examples, Heuristics, [{Feature, BestValue}|Acc]).

best_split_value(_, []) ->
    rr_candidate:new(0, none, []);
best_split_value(#rr_heuristic{evaluator=Evaluator} = Heuristics, 
		 [{OldValue0, OldCovered0}|Splits]) ->
    lists:foldl(fun({Value, Covered}, Candidate) ->
			He = rr_heuristic:update(Covered, Heuristics),
			Score = Evaluator:evaluate(He),
			case Score > rr_candidate:score(Candidate) of
			    true ->
				rr_candidate:new(Score, Value, Covered);
			    false ->
				Candidate
			end
		end, rr_candidate:new(Evaluator:evaluate(rr_heuristic:update(OldCovered0, Heuristics)),
				      OldValue0, OldCovered0), Splits).
    
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
    File = csv:reader("data/car.txt"),
    {Features, Examples} = rr_example:load(File, 4),
    ruleset(Features, Examples).
   
