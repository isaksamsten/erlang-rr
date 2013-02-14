%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_tree).
-compile(export_all).

test(File) ->
    Csv = csv:reader(File),
    {Features, Examples} = rr_example:load(Csv, 4),
    generate_model(Features, Examples).


generate_model(Features, Examples) ->
    Total = rr_example:count(Examples),
    evaluate_split(Features, Examples, Total, []).

evaluate_split([], _, _, Acc) ->
    lists:reverse(Acc);
evaluate_split([F|Features], Examples, Total, Acc) ->
    Split = rr_example:split(F, Examples),
    evaluate_split(Features, Examples, Total,
		   [info(Split, Total) / (split_info(Split, Total) + 0.000000000000000001)|Acc]).



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
    
    
