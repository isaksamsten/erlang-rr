%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% Functions for handling missing values. Each function should return
%%% either: {true, {ExId, ExIdCount}} for an example to be distributed
%%% to the left or: {false, {ExId, ExIdCount}} for an example to be
%%% distributed to the right or: ignore to ignore an example with
%%% missing values
%%%
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_missing).
-compile(export_all).

partition(ExId, Fraction) ->
    Count = rr_example:count(ExId),
    LeftCount = Count * Fraction,
    RightCount = Count - LeftCount,
    Id = rr_example:exid(ExId),
    {both, {{Id, LeftCount}, {Id, RightCount}}}.

random_partition(build, _, ExId, _, _) ->
    partition(ExId, 0.5);
random_partition(predict, _, ExId, _, _) ->
    {direction(random:uniform()) =< 0.5, {rr_example:exid(ExId), rr_example:count(ExId)}}.

weighted_partition(build, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    partition(ExId, LeftFraction);
weighted_partition(predict, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {direction(random:uniform() =< LeftFraction), {rr_example:exid(ExId), rr_example:count(ExId)}}.
    
weighted(_, _, ExId, NoLeft, NoRight) ->
    LeftFraction = case NoLeft of 0 -> 0; 0.0 -> 0; _-> (NoLeft) / (NoLeft + NoRight) end,
    {direction(LeftFraction >= 0.5), {rr_example:exid(ExId), rr_example:count(ExId)}}.
		       


%%
%% Distribute the examples evenly over the left and right side
%%
random(_, _, ExId, _, _) ->
    {direction(random:uniform() =< 0.5), {rr_example:exid(ExId), rr_example:count(ExId)}}.

%%
%% Distribute examples based on the number of examples falling in each
%% branch.
%%
random_weighted(_, _, ExId, NoLeft, NoRight) ->
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {direction(random:uniform() =< LeftFraction), {rr_example:exid(ExId), rr_example:count(ExId)}}.

ignore(_, _, _, _, _) ->
    ignore.


%%
%% Distribute missing values by sampling from the proximate examples
%%
proximity(_, {{Type, Feature}, Value} = F, ExId, NoLeft, NoRight) ->
    Prox = rr_proximity:examples(ExId),
    Avg = average_proximity(Type, Feature, Prox, 5),
    case Avg of
	 '?' ->
	    weighted(build, F, ExId, NoLeft, NoRight);
	 Avg ->
	    {direction(Type, Value, Avg), exid(ExId)}
    end.    

%%
%% NOTE: this is really bad..
%%
average_proximity(numeric, FeatureId, Prox, N) ->
    mean_proximity(Prox, FeatureId, N, []);
average_proximity(categoric, FeatureId, Prox, N) ->
    mode_proximity(Prox, FeatureId, N, dict:new()).

mode_proximity(Prox, FeatureId, N, Dict) -> 
    case Prox of
	[] ->
	    [{F, _}|_] = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
	    F;
	_ when N == 0 ->
	    [{F, _}|_] = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
	    F;
	['?'|Rest] ->
	    io:format(standard_error, "Missing value... ~n", []),
	    mode_proximity(Rest, FeatureId, N, Dict);
	[{P, _Score}|Rest] ->
	    mode_proximity(Rest, FeatureId, N, dict:update_counter(P, 1, Dict))
    end.
	    
mean_proximity([], _, _, List) ->
    case List of
	[] ->
	    '?';
	_ ->
	    lists:sum(List) / length(List)
    end;
mean_proximity(_, _, 0, List) ->
    case List of
	[] ->
	    '?';
	_ ->
	    lists:sum(List) / length(List)
    end;
mean_proximity([{Proximity, _Score}|Rest], FeatureId, N, Acc) ->
    Value = rr_example:feature(Proximity, FeatureId),
    case Value of
	'?' ->
	    mean_proximity(Rest, FeatureId, N - 1, Acc);
	_ ->
	    mean_proximity(Rest, FeatureId, N - 1, [Value|Acc])
    end.
	  
    

%%
%% Distribute every example in the right branch (i.e. consider it
%% false)
%%
right(_, _, ExId, _, _) ->
    {right, exid(ExId)}.
left(_, _, ExId, _, _) ->
    {left, exid(ExId)}.


exid(ExId) ->
    {rr_example:exid(ExId), rr_example:count(ExId)}.


direction(categoric, Value, Avg) ->
    direction(Value == Avg);
direction(numeric, Value, Avg) ->
    direction(Value >= Avg).

direction(true) ->
    left;
direction(false) ->
    right.

