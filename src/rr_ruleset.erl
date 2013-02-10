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
    H = rr_heuristic:new(rr_laplace, rr_example:classes(Examples)),
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
    Heu = Heuristics#rr_heuristic{pos=Pos, neg=Neg, apriori=Pos/(Pos+Neg)},

    Rule = learn_rule_for_class(Features, Class, Binary, Heu, []),
    learn_rules_for_class(Features, Rest, Examples, Heuristics, [Rule|Ruleset]).

learn_rule_for_class(Features, Class, Examples, Heuristics, ClassRules) ->
    {Rule, Covered} = separate_and_conquer(Features, Class, Examples, Heuristics),
    NotCovered = remove_covered(Examples, Covered),
  
    Pos = rr_example:count('+', NotCovered),
%    Neg = rr_example:count('-', NotCovered),
 %   Heu = rr_heuristic:const(Pos, Neg, Heuristics), %% NOTE???

    case rr_rule:purity(Rule) > 0.75 of
	true -> 
	    case Pos > 0 of
		true ->
		    learn_rule_for_class(Features, Class, NotCovered, Heuristics, [Rule|ClassRules]);
		false ->
		    [Rule|ClassRules]
	    end;
	false ->  
	    ClassRules
    end.

coverage(NotCovered) ->
    {rr_example:count('+', NotCovered), rr_example:count('-', NotCovered)}.

separate_and_conquer(Features, Class, Examples, Heuristics) ->
    separate_and_conquer(Features, Class, Examples, Heuristics, rr_rule:new(Class)).

separate_and_conquer([], _, NotCovered, _, Rules) ->
    {rr_rule:sort(Rules), NotCovered};    
separate_and_conquer(Features, Class, Examples, Heuristics, Rules) ->
    {{Score, {_Pos, Neg} = Coverage}, {Feature, _} = Condition, Covered} = learn_one_rule(Features, Examples, Heuristics),
    Rules0 = rr_rule:add(Rules, Condition, Score, Coverage),

    case Score >= rr_rule:score(Rules) of
	true ->
	    case Neg == 0 of
		true ->
		    {rr_rule:sort(Rules0), Covered};
		false ->
		    separate_and_conquer(Features -- [Feature], Class, Covered, Heuristics, Rules0)
	    end;
	false ->
	    {rr_rule:sort(Rules), Covered}
    end.

%%
%% Learn the best possible Rule from Features and Examples
%% 		      
learn_one_rule(Features, Examples, Heuristics) ->
    {Feature, Candidate} = find_best_subspace(Features, Examples, Heuristics),
    Covered = rr_candidate:coverage(Candidate),
    Value = rr_candidate:value(Candidate),
    Score = rr_candidate:score(Candidate),

    {{Score, coverage(Covered)}, {Feature, Value}, Covered}.

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

test(File) ->
    rr_example:init(),
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    ruleset(Features, Examples).
   
