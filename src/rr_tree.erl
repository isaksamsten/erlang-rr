%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_tree).
-compile(export_all).

-include("rr_tree.hrl").

test(File) ->
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    {Train, Test} = rr_example:split_dataset(Examples, 0.66),
    Conf = #rr_conf{
	      prune = log_stop(20),
	      selection = fun best_feature_selection/1,
	      depth=0
	     },

    Model = rr_ensamble:generate_model(Features, Train, Conf, rr_example:count(Examples)),
    io:format("~p ~n", [Model]),

    Dict = rr_ensamble:evaluate_model(Model, Test, Conf),
    io:format("Accuracy: ~p ~n", [rr_eval:accuracy(Dict)]).



best_feature_selection([Candidate]) ->
    Candidate;
best_feature_selection(Candidates) ->
    hd(sort_candidates(Candidates)).
   
random_feature_selection([Candidate]) ->
    Candidate;
random_feature_selection(Candidates) ->
    Index = random:uniform(length(Candidates)),
    lists:nth(Index, Candidates).

log_stop(Initial) ->
    fun (Examples) ->
	    Examples > Initial
    end.

generate_model(Features, Examples, Conf) ->
    random:seed(now()),
    build_decision_tree(Features, Examples, Conf).
    

evaluate_model(Model, Examples, Conf) ->
    lists:foldl(fun({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Model, Conf, Acc)
		end, dict:new(), Examples).

predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    Prediction = make_prediction(Model, Example, Conf),
    predict_all(Actual, Rest, Model, Conf,
		dict:update(Actual, fun (Predictions) ->
					    [Prediction|Predictions]
				    end, [Prediction], Dict)).

make_prediction(Model, Example, Conf) ->
    Attributes = rr_example:example(Example),
    predict(Attributes, Model, Conf).

predict(_, #rr_leaf{class=Class, purity=P}, _) ->
    {Class, P};
predict(Attributes, #rr_node{feature={categoric, Id}, popularity=Other, nodes=Nodes}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    case lists:keyfind(Value, 1, Nodes) of
	{Value, Node} ->
	    predict(Attributes, Node, Conf);
	false ->
	    {Popular, _} = hd(Other),
	    predict(Attributes, case lists:keyfind(Popular, 1, Nodes) of
				    {Popular, Node} -> 
					Node;
				    _ -> throw({error})
				end, Conf)
    end;
predict(Attributes, #rr_node{feature={{numeric, Id}, T}, nodes=Nodes}, Conf) ->
    Value = rr_example:feature(Attributes, Id),
    Gt = lists:keyfind('>=', 1, Nodes),
    Lt = lists:keyfind('<', 1, Nodes),
    case Value >= T of
	true ->
	    predict(Attributes, case Gt of
				    false -> element(2, Lt);
				    {_, Node} -> Node
				end, Conf);
	false ->
	    predict(Attributes, case Lt of
				    false -> element(2, Gt);
				    {_, Node} -> Node
				end, Conf)
    end.
	    
		    

build_decision_tree([], [], _) ->
    make_leaf([], error);
build_decision_tree([], Examples, _) ->
    make_leaf(Examples, rr_example:majority(Examples));
build_decision_tree(_, [{Class, Count, _ExampleIds}] = Examples, _) ->
    make_leaf(Examples, {Class, Count});
build_decision_tree(Features, Examples, #rr_conf{prune=Prune, depth=Depth} = Conf) ->
    NoExamples = rr_example:count(Examples),
    case Prune(Depth) of
	true ->
	    io:format("~p ~n", [Examples]),
	    make_leaf(Examples, rr_example:majority(Examples));
	false ->
	    Log = round((math:log(length(Features)) / math:log(2))) + 1,
	    case rr_example:random_features(Features, Log) of
		[] ->
		    make_leaf(Examples, rr_example:majority(Examples));
		[_] ->
		    make_leaf(Examples, rr_example:majority(Examples));
		Features0  -> 
		    Candidate = evaluate_split(Features0, Examples, NoExamples, Conf),
		    Nodes = build_decision_branches(Features, Candidate, Conf#rr_conf{depth=Depth + 1}),
		    make_node(Candidate, Nodes)
	    end	   
    end.

build_decision_branches(Features, #rr_candidate{feature={categoric, _} = Feature, split=Split}, Conf) ->
    %Features0 = Features -- [Feature],
    build_decision_branches(Features, Split, Conf, []);
build_decision_branches(Features, #rr_candidate{feature={{numeric, _} = Feature, _}, split=Split}, Conf) ->
    %Features0 = if length(Split) == 1 ->
%			Features -- [Feature];
%		   true ->
%			Features
%		end,
    build_decision_branches(Features, Split, Conf, []).



build_decision_branches(_, [], _, Acc) ->
    Acc;
build_decision_branches(Features, [{Value, Split}|Rest], Conf, Acc) ->
    Node = build_decision_tree(Features, Split, Conf),
    build_decision_branches(Features, Rest, Conf, [{Value, Node}|Acc]).


make_node(#rr_candidate{feature=Feature, score=Score, split=Splits}, Nodes) ->
    Popularity = lists:sort(fun(A, B) ->
				    A > B
			    end, [{Value, rr_example:count(Split)} || {Value, Split} <- Splits]),
    #rr_node{score=Score, feature=Feature, popularity=Popularity, nodes=Nodes}.

make_leaf([], Class) ->
    #rr_leaf{purity=0, correct=0, incorrect=0, class=Class};
make_leaf(Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rr_leaf{purity=C/N, correct=C, incorrect=N-C, class=Class}.


evaluate_split(Features, Examples, Total, Conf) ->
    evaluate_split(Features, Examples, Total, Conf, []).
evaluate_split([], _, _, #rr_conf{selection=Selection}, Acc) ->
    Selection(Acc);
evaluate_split([F|Features], Examples, Total, Conf, Acc) ->
    Cand = case rr_example:split(F, Examples) of
	       {Threshold, Split} ->
		   #rr_candidate{feature = {F, Threshold}, 
				 score = gain_ratio(Split, Total), 
				 split = Split};
	       Split ->
		   #rr_candidate{feature = F, 
				 score = gain_ratio(Split, Total), 
				 split = Split}
	   end,
    evaluate_split(Features, Examples, Total, Conf, [Cand|Acc]).

sort_candidates(Acc) ->
    lists:sort(fun(A, B) ->
		       A#rr_candidate.score < B#rr_candidate.score
	       end, Acc).

entropy(Examples) ->
    Counts = [C || {_, C, _} <- Examples],
    entropy(Counts, lists:sum(Counts)).

entropy(Counts, Total) ->
    -1 * lists:foldl(fun (0, Count) ->
			     Count;
			 (Class, Count) ->
			     Fraction = Class / Total,
			     Count + Fraction * math:log(Fraction)/math:log(2)
		     end, 0, Counts).

gain_ratio(Split, Total) ->
    Info = info(Split, Total),
    case split_info(Split, Total) of
	0.0 ->
	    Info;
	SplitInfo ->
	    Info / SplitInfo
    end.

info(ValueSplits, Total) ->
    info(ValueSplits, Total, 0).

info([], _, Acc) -> Acc;
info([{_Value, Splits}|Rest], Total, Acc) ->
    ClassSum = rr_example:count(Splits),
    info(Rest, Total,
	 Acc + (ClassSum / Total) * entropy(Splits)).

split_info(ValueSplits, Total) ->
    Examples = [Example || {_Value, Example} <- ValueSplits],
    Sums = [rr_example:count(E) || E <- Examples],
    split_info(Sums, Total, 0).

split_info([], _, Acc) -> 
    -1 * Acc;
split_info([ExampleCount|Rest], Total, Acc) ->
    split_info(Rest, Total,
	 Acc + case ExampleCount > 0 of
		   true -> (ExampleCount/Total) * math:log(ExampleCount/Total);
		   false -> 0
	       end).
    
    
