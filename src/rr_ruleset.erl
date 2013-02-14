%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% Implementaion of a ruleset
%%% TODO: information gain as score measure
%%%
%%% @end
%%% Created :  5 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_ruleset).
-author('isak-kar@dsv.su.se').
-include("rr.hrl").

-compile(export_all).

%%
%% Laplace estimate
%%
laplace(#rr_conf{covered={Pos, Neg}, classes=Classes}) ->
    (Pos + 1) / (Pos + Neg + Classes).

m_estimate(M) ->
    fun(#rr_conf{covered={Pos, Neg}, original={P, N}}) ->
	    (Pos + M * (P / (P + N))) / (Pos + Neg + M)
    end.

%%
%% Rule accuacy
%%
accuracy(#rr_conf{covered={Pos, Neg}, original={P, N}}) ->
    (Pos + (N - Neg))/(P+N).

%%
%% Rule purity measure
%%
purity(#rr_conf{covered={Pos, Neg}}) ->
    Pos / (Pos + Neg).

purity_stop(#rr_rule{covered={Pos, Neg}}, #rr_conf{original={P, N}}) ->
    Pos / (Pos + Neg) > P / (P + N).

%%
%% Best first search. Only search the locally optimal rule
%%
best_first_search(Candidates) ->
    hd(sort_candidates(Candidates)).

%%
%% Stochastically search on of the top 1/3 of possible candidates
%%
stochastic_best_search(Candidates) ->
    case length(Candidates) of
	X when X >= 3 ->
	    C = sort_candidates(Candidates),
	    Index = random:uniform(X div 3),
	    lists:nth(Index, C);
	2 ->
	    lists:nth(random:uniform(2), Candidates);
	1 -> 
	    hd(Candidates)
    end.

stochastic_search(Candidates) ->
    case length(Candidates) of
	X when X >= 3 ->
	    Index = random:uniform(X),
	    lists:nth(Index, Candidates);
	2 ->
	    lists:nth(random:uniform(2), Candidates);
	1 -> 
	    hd(Candidates)
    end.

	    
%%
%% Generate a ruleset from "Features" and "Examples"
%%
generate_model(Features, Examples) ->
    {Default, Rest} = default_class(Examples),
    H = #rr_conf{eval   = fun laplace/1,
		 search = fun best_first_search/1,
		 stop   = fun purity_stop/2,
		 classes=rr_example:classes(Examples)},
    DefaultRule = default_rule(Default, Examples, H),
    learn_rules_for_class(Features, Rest, Examples, H, []) ++ DefaultRule.

    
evaluate_model(Model, Examples) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Model, Acc)
		end, dict:new(), Examples).

evaluate_model2(Model, Examples) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all2(Class, ExampleIds, Model, Acc)
		end, dict:new(), Examples).

predict_all(_, [], _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Dict) ->
    io:format("Actual: ~p ", [Actual]),
    Prediction = predict_majority(Model, rr_example:example(Example), []),
    predict_all(Actual, Rest, Model, dict:update(Actual, fun(Predictions) ->
								 [Prediction|Predictions]
							 end, [Prediction], Dict)).

predict_all2(_, [], _, Dict) ->
    Dict;
predict_all2(Actual, [Example|Rest], Model, Dict) ->
    Prediction = predict(rr_example:example(Example), Model),
    predict_all2(Actual, Rest, Model, dict:update(Actual, fun(Predictions) ->
								 [Prediction|Predictions]
							 end, [Prediction], Dict)).
	
predict_majority(0, _, Acc) ->
    {{C, N}, Dict} = majority(Acc),
    io:format("Majority: ~p ~p ~p \n", [C, N, dict:to_list(Dict)]),
    {C, N};
predict_majority(N, Attr, Acc) ->
    [{_, Model}|_] = ets:lookup(models, N),
    predict_majority(N - 1, Attr, [predict(Attr, Model)|Acc]).

majority(Acc) ->
    Dict = lists:foldl(fun ({Item, Prob}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1 end, 1, Dict)
		       end, dict:new(), Acc),
    {dict:fold(fun (Class, Count, {MClass, MCount}) ->
		      case Count > MCount of
			  true -> {Class, Count};
			  false -> {MClass, MCount}
		      end
	      end, {undefined, 0}, Dict), Dict}.
    

predict(Attributes, [Rules|Rest]) ->
    case predict_rules(Attributes, Rules) of
	{ok, Prediction} ->
	    Prediction;
	error ->
	    predict(Attributes, Rest)
    end.

predict_rules(_, []) ->
    error;
predict_rules(Attributes, [#rr_rule{score=Score,
				    antecedent=A,
				    consequent=Class}|Rules]) ->
    case evaluate_antecedent(Attributes, A) of
	true ->
	    {ok, {Class, Score}};
	false ->
	    predict_rules(Attributes, Rules)
    end.

evaluate_antecedent(_, []) ->
    true;
evaluate_antecedent(_, ['$default$']) ->
    true;
evaluate_antecedent(Attributes, [{{categoric, Feature}, PredictValue}|Rest]) ->
    ActualValue = rr_example:feature(Attributes, Feature),
    case PredictValue == ActualValue of
	true ->
	    evaluate_antecedent(Attributes, Rest);
	false ->
	    false
    end.

%%
%% Learn rules for one class at a time
%%
learn_rules_for_class(_, [], _, _, Ruleset) ->
    lists:reverse(Ruleset);
learn_rules_for_class(Features, [Class|Rest], Examples, Conf, Ruleset) ->
    Binary = rr_example:to_binary(Class, Examples),
    Heu = Conf#rr_conf{original=rr_example:coverage(Binary)},

    Rule = learn_rule_for_class(Features, Class, Binary, Heu, []),
    learn_rules_for_class(Features, Rest, Examples, Heu, [Rule|Ruleset]).

%%
%% Learn a rule for class "Class"
%%
learn_rule_for_class(Features, Class, Examples, #rr_conf{stop=Stop} = Conf, ClassRules) ->
    {Rule, Covered} = separate_and_conquer(Features, Class, Examples, Conf),
    case Stop(Rule, Conf) of
	true -> 
	    NotCovered = rr_example:remove_covered(Examples, Covered),
  	    Pos = rr_example:count('+', NotCovered),
	    case Pos > 0 of
		true ->
		    learn_rule_for_class(Features, Class, NotCovered, Conf, [Rule|ClassRules]);
		false ->
		    [Rule|ClassRules]
	    end;
	false ->  
	    ClassRules
    end.

separate_and_conquer(Features, Class, Examples, Conf) ->
    separate_and_conquer(Features, Class, Examples, Conf, #rr_rule{consequent=Class}).

separate_and_conquer([], _, NotCovered, _, Rules) ->
    {reverse_antecedents(Rules), NotCovered};    
separate_and_conquer(Features, Class, Examples, Conf, Rules) ->
    {{Score, {Pos, Neg}}, {Feature, _} = Condition, Covered} = learn_one_rule(Features, Examples, Conf),
    case Score >= Rules#rr_rule.score of
	true ->
	    Rules0 = add_antecedent(Rules, Condition, Score, {Pos, Neg}),
	    case Neg =< 0 of
		true ->
		    {reverse_antecedents(Rules0), Covered};
		false ->
		    separate_and_conquer(Features -- [Feature], Class, Covered, Conf, Rules0)
	    end;
	false ->
	    {reverse_antecedents(Rules), Covered}
    end.

%%
%% Learn the possible "Rule" from "Features" and "Examples"
%%
learn_one_rule(Features, Examples, Conf) ->
    {Feature, #rr_candidate{covered=Covered,
			    value=Value,
			    score=Score}} = find_subspace(Features, Examples, Conf),
    {{Score, rr_example:coverage(Covered)}, {Feature, Value}, Covered}.


%%
%% Find the best subspace to cover (i.e. what feature is the best)
%%
find_subspace(Features, Examples, Heuristics) ->
    find_subspaces(Features, Examples, Heuristics, []).

find_subspaces([], _, #rr_conf{search=Search}, Acc) ->
    Search(Acc);   
find_subspaces([Feature|Features], Examples, Heuristics, Acc) ->
    Split = rr_example:split(Feature, Examples),
    BestValue = best_split_value(Heuristics, Split),
    find_subspaces(Features, Examples, Heuristics, [{Feature, BestValue}|Acc]).

best_split_value(_, []) ->
    #rr_candidate{score=0};
best_split_value(#rr_conf{eval=Eval} = H, [{Value0, Covered0}|Splits]) ->
    Score0 = Eval(H#rr_conf{covered=rr_example:coverage(Covered0)}),
    lists:foldl(fun({Value, Covered}, Candidate) ->
			Score = Eval(H#rr_conf{covered=rr_example:coverage(Covered)}),
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

%%
%% Generate a default rule for "Class"
%%
default_rule(Class, Examples, #rr_conf{eval=Eval} = Conf) ->
    Binary = rr_example:to_binary(Class, Examples),
    Coverage = rr_example:coverage(Binary),
    Score = Eval(Conf#rr_conf{original=Coverage, covered=Coverage}),
    [[#rr_rule{score=Score, covered=Coverage, antecedent=['$default$'], consequent=Class}]].
    

%%
%% Sort candidate rules according to their score. Larger is better.
%%
sort_candidates(Candidates) ->
    lists:sort(fun({_, #rr_candidate{score=Ca}}, 
		   {_, #rr_candidate{score=Cb}}) ->
		       Ca > Cb
	       end, Candidates).

%%
%% Add antecedent to a rule
%%
add_antecedent(#rr_rule{antecedent=A, length=L} = Rule, Condition, Score, Coverage) ->
    Rule#rr_rule{antecedent=[Condition|A], length=L+1, score=Score, covered=Coverage}.

%%
%% Reverse the antecedents of a rule (since they are inserted in order)
%%
reverse_antecedents(#rr_rule{antecedent=A} = Rule) ->
    Rule#rr_rule{antecedent=lists:reverse(A)}.


test(File, Classifier) ->
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    {Train, Test} = rr_example:split_dataset(Examples, 0.66),
%    io:format("~p", [generate_model(Features, Examples)]).
    ets:new(models, [public, named_table]),
    ets:new(predictions, [public, named_table]),

    spawn_ruleset_classifiers(Classifier, 4, Features, Train, lists:sum([C || {_, C, _} <- Examples])),
    Dict = evaluate_model(Classifier, Test),
    io:format("Accuracy: ~p ~n", [rr_eval:accuracy(Dict)]).

spawn_ruleset_classifiers(Sets, Cores, Features, Examples, MaxId) ->
    Self = self(),
    [spawn_link(fun() ->
			<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
			random:seed({A,B,C}),
			ruleset_generator_process(Self, MaxId)
		end) || _ <- lists:seq(1, Cores)],
    ruleset_classification_coordinator(Self, Sets, Cores, Features, Examples).

ruleset_classification_coordinator(_, 0, 0, _, _) ->
    done;
ruleset_classification_coordinator(Self, 0, Cores, Features, Examples) ->
    receive
	{more, Self, Pid} ->
	    Pid ! {exit, Self},
	    ruleset_classification_coordinator(Self, 0, Cores, Features, Examples);
	done ->
	    ruleset_classification_coordinator(Self, 0, Cores - 1, Features, Examples)
    end;	    
ruleset_classification_coordinator(Self, Sets, Cores, Features, Examples) ->
    receive 
	{more, Self, Pid} ->
	    Pid ! {batch, Sets, Features, Examples},
	    ruleset_classification_coordinator(Self, Sets - 1, Cores, Features, Examples)
    end.

ruleset_generator_process(Parent, MaxId) ->
    Parent ! {more, Parent, self()},
    receive
	{batch, Id, Features, Examples} ->
	    Log = (length(Features) div 2) + 1, %round((math:log(length(Features)) / math:log(2))) + 1,
	    Features0 = rr_example:random_features(Features, Log),
	    {Bag, OutBag} = rr_example:bootstrap_replicate(Examples, MaxId),
	    Model = generate_model(Features0, Bag),
%	    io:format("~p", [Model]),
	    Dict = evaluate_model2(Model, OutBag),
	    io:format("Building model ~p (OOB accuracy: ~p) ~n", [Id, rr_eval:accuracy(Dict)]),
	    ets:insert(models, {Id, Model}),
	    ruleset_generator_process(Parent, MaxId);
	{exit, Parent} ->
	    Parent ! done
    end.
