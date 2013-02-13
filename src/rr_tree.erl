%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Basic (simple) tree induction algorithm
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_tree).
-compile(export_all).

generate_model(Features, Examples) ->
    ok.



entropy(Examples) ->
    Counts = [C || {_, C, _} <- Examples],
    entropy(Counts, lists:sum(Counts)).

entropy(Counts, Total) ->
    -1 * lists:foldl(fun (0, Count) ->
			     Count;
			 (Class, Count) ->
			     Fraction = Class / Total,
			     Count + Fraction * math:log(Fraction)
		     end, 0, Counts).

gain(ValueSplits, Total) ->
    gain(ValueSplits, Total, 0).

gain([], _, Acc) -> Acc;
gain([{_Value, Splits}|Rest], Total, Acc) ->
    0.
