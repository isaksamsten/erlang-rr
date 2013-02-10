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

laplace(#rr_heuristic{covered={Pos, Neg}, classes=Classes}) ->
    (Pos + 1) / (Pos + Neg + Classes).

accuracy(#rr_heuristic{covered={Pos, Neg}, original={P, N}}) ->
    (Pos + (N - Neg))/(P+N).

purity(#rr_heuristic{covered={Pos, Neg}}) ->
    Pos / (Pos + Neg).

best_first_search(Candidates) ->
    hd(sort_candidates(Candidates)).

stochastic_search(Candidates) ->
    case length(Candidates) of
	X when X >= 3 ->
	    C = rr_candidate:sort(Candidates),
	    Index = random:uniform(X div 3),
	    lists:nth(Index, C);
	2 ->
	    lists:nth(random:uniform(2), Candidates);
	1 -> 
	    hd(Candidates)
    end.
	    

ruleset(Features, Examples) ->
    random:seed(now()),
    {Default, Rest} = default_class(Examples),
    H = #rr_heuristic{eval=fun laplace/1, 
		      search=fun stochastic_search/1,
		      classes=rr_example:classes(Examples)},
    Ruleset = learn_rules_for_class(Features, Rest, Examples, H, []),
    Ruleset ++ [{'$default$', Default}].

%%
%% Learn rule for one class at a time
%%
learn_rules_for_class(_, [], _, _, Ruleset) ->
    lists:reverse(Ruleset);
learn_rules_for_class(Features, [Class|Rest], Examples, Heuristics, Ruleset) ->
    Binary = rr_example:to_binary(Class, Examples),
    Heu = Heuristics#rr_heuristic{original=rr_example:coverage(Binary)},

    Rule = learn_rule_for_class(Features, Class, Binary, Heu, []),
    learn_rules_for_class(Features, Rest, Examples, Heu, [Rule|Ruleset]).

learn_rule_for_class(Features, Class, Examples, Heuristics, ClassRules) ->
    {Rule, Covered} = separate_and_conquer(Features, Class, Examples, Heuristics),
    case rr_rule:purity(Rule) > 0.7 of
	true -> 
	    NotCovered = rr_example:remove_covered(Examples, Covered),
  	    Pos = rr_example:count('+', NotCovered),
	    case Pos > 0 of
		true ->
		    learn_rule_for_class(Features, Class, NotCovered, Heuristics, [Rule|ClassRules]);
		false ->
		    [Rule|ClassRules]
	    end;
	false ->  
	    ClassRules
    end.

separate_and_conquer(Features, Class, Examples, Heuristics) ->
    separate_and_conquer(Features, Class, Examples, Heuristics, rr_rule:new(Class)).

separate_and_conquer([], _, NotCovered, _, Rules) ->
    {rr_rule:reverse(Rules), NotCovered};    
separate_and_conquer(Features, Class, Examples, Heuristics, Rules) ->
    {{Score, {Pos, Neg}}, {Feature, _} = Condition, Covered} = learn_one_rule(Features, Examples, Heuristics),

    case Score >= rr_rule:score(Rules) of
	true ->
	    Rules0 = rr_rule:add(Rules, Condition, Score, {Pos, Neg}),
	    case Neg == 0 of
		true ->
		    {rr_rule:reverse(Rules0), Covered};
		false ->
		    separate_and_conquer(Features -- [Feature], Class, Covered, Heuristics, Rules0)
	    end;
	false ->
	    {rr_rule:reverse(Rules), Covered}
    end.

%%
%% Learn the best possible Rule from Features and Examples
%% 		      
learn_one_rule(Features, Examples, Heuristics) ->
    {Feature, #rr_candidate{covered=Covered,
			    value=Value,
			    score=Score}} = find_subspace(Features, Examples, Heuristics),
    {{Score, rr_example:coverage(Covered)}, {Feature, Value}, Covered}.


%%
%% Find the best subspace to cover (i.e. what feature is the best)
%%
find_subspace(Features, Examples, Heuristics) ->
    find_subspaces(Features, Examples, Heuristics, []).

find_subspaces([], _, #rr_heuristic{search=Search}, Acc) ->
    Search(Acc);   
find_subspaces([Feature|Features], Examples, Heuristics, Acc) ->
    Split = rr_example:split(Feature, Examples),
    BestValue = best_split_value(Heuristics, Split),
    find_subspaces(Features, Examples, Heuristics, [{Feature, BestValue}|Acc]).

best_split_value(_, []) ->
    #rr_candidate{score=0};
best_split_value(#rr_heuristic{eval=Eval} = H, [{Value0, Covered0}|Splits]) ->
    Score0 = Eval(H#rr_heuristic{covered=rr_example:coverage(Covered0)}),
    lists:foldl(fun({Value, Covered}, Candidate) ->
			Score = Eval(H#rr_heuristic{covered=rr_example:coverage(Covered)}),
			case Score > Candidate#rr_candidate.score of
			    true ->
				#rr_candidate{score=Score, value=Value, covered=Covered};
			    false ->
				Candidate
			end
		end, #rr_candidate{score=Score0, value=Value0, covered=Covered0}, Splits).
    
%%
%% Returns the {D, C} where D is the default class (with the higest a
%% priori probability) and C is a (ordered) list of classes with the
%% least probable first
%%
default_class(Examples) ->
    [Largest|Rest] = [Class || {Class, _, _} <- lists:reverse(lists:keysort(2, Examples))],
    {Largest, lists:reverse(Rest)}.

sort_candidates(Candidates) ->
    lists:sort(fun({_, #rr_candidate{score=Ca}}, 
		   {_, #rr_candidate{score=Cb}}) ->
		       Ca > Cb
	       end, Candidates).


test(File) ->
    rr_example:init(),
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    ruleset(Features, Examples).
   
