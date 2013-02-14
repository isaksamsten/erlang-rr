%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_tree).
-compile(export_all).

-include("rr.hrl").
-include("rr_tree.hrl").

test(File) ->
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    generate_model(Features, Examples).


log_stop(Initial) ->
    Initial0 = trunc(math:log(Initial)/math:log(2)) + 1,
    io:format("Initial: ~p ~n", [Initial0]),
    fun (Examples) ->
	    Examples < Initial0
    end.

generate_model(Features, Examples) ->
    build_decision_tree(Features, Examples, 
			#rr_conf{stop=log_stop(rr_example:count(Examples))}).

build_decision_tree([], [], _) ->
    make_leaf([], error);
build_decision_tree([], Examples, _) ->
    make_leaf(Examples, rr_example:majority(Examples));
build_decision_tree(_, [{Class, Count, _ExampleIds}] = Examples, _) ->
    make_leaf(Examples, {Class, Count});
build_decision_tree(Features, Examples, #rr_conf{stop=Stop} = Conf) ->
    NoExamples = rr_example:count(Examples),
    case Stop(NoExamples) of
	true ->
	    make_leaf(Examples, rr_example:majority(Examples));
	false ->
	    Cand = hd(evaluate_split(Features, Examples, NoExamples, [])),
	    Features0 = Features -- [Cand#rr_candidate.feature],
	    Nodes = [{Value, build_decision_tree(Features0, Split, Conf)} || {Value, Split} <- Cand#rr_candidate.split],
	    make_node(Cand, Nodes)
    end.

make_node(#rr_candidate{feature=Feature, score=Score}, Nodes) ->
    #rr_node{score=Score, feature=Feature, nodes=Nodes}.

make_leaf([], Class) ->
    #rr_leaf{purity=0, correct=0, incorrect=0, class=Class};
make_leaf(Covered, {Class, C}) ->
    N = rr_example:count(Covered),
    #rr_leaf{purity=C/N, correct=C, incorrect=N-C, class=Class}.

evaluate_split([], _, _, Acc) ->
    lists:sort(fun(A, B) ->
		       A#rr_candidate.score < B#rr_candidate.score
	       end, Acc);
evaluate_split([F|Features], Examples, Total, Acc) ->
    Split = rr_example:split(F, Examples),
    Info = info(Split, Total),
    GainRatio = case split_info(Split, Total) of
		    0.0 ->
			Info;
		    SplitInfo ->
			Info / SplitInfo
		end,
    evaluate_split(Features, Examples, Total,
		   [#rr_candidate{feature = F, 
				  score = GainRatio, 
				  split = Split}|Acc]).



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
    
    
