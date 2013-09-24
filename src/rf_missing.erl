%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% Functions for handling missing values. Each function should return
%%% either: {left, {ExId, ExIdCount}} for an example to be distributed
%%% to the left or: {right, {ExId, ExIdCount}} for an example to be
%%% distributed to the right or: ignore to ignore an example with
%%% missing values
%%%
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rf_missing).
-compile(export_all).
-export([
	 random_partition/0
	]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").


-spec partition(exid(), float()) -> missing_example().
partition(ExId, Fraction) ->
    Count = rr_example:count(ExId),
    LeftCount = Count * Fraction,
    RightCount = Count - LeftCount,
    Id = rr_example:exid(ExId),
    {both, {{Id, LeftCount}, {Id, RightCount}}}.

%% @doc random partitions
-spec random_partition() -> missing_fun().
random_partition() ->
    fun random_partition/6.
			      

%% @private
random_partition(build, _, _, ExId, _, _) ->
    partition(ExId, 0.5);
random_partition(predict, _, _, ExId, _, _) ->
    {direction(random:uniform() =< 0.5), 
     {rr_example:exid(ExId), rr_example:count(ExId)}}.
weighted_partition(build, _, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    partition(ExId, LeftFraction);
weighted_partition(predict, _, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {direction(random:uniform() =< LeftFraction), 
     {rr_example:exid(ExId), rr_example:count(ExId)}}.
    
weighted(_, _, _, ExId, NoLeft, NoRight) ->
    LeftFraction = case NoLeft of
		       0 -> 0; 0.0 -> 0; 
		       _-> (NoLeft) / (NoLeft + NoRight)
		   end,
    {direction(LeftFraction >= 0.5), 
     {rr_example:exid(ExId), rr_example:count(ExId)}}.
		       

%%
%% Distribute the examples evenly over the left and right side
%%
random(_, _, _, ExId, _, _) ->
    {direction(random:uniform() =< 0.5),
     {rr_example:exid(ExId), rr_example:count(ExId)}}.

%%
%% Distribute examples based on the number of examples falling in each
%% branch.
%%
random_weighted(_, _, _, ExId, NoLeft, NoRight) ->
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {direction(random:uniform() =< LeftFraction), 
     {rr_example:exid(ExId), rr_example:count(ExId)}}.

ignore(_, _, _, _, _, _) ->
    ignore.
	  
%%
%% Distribute every example in the right branch (i.e. consider it
%% false)
%%
right(_, _, _, ExId, _, _) ->
    {right, exid(ExId)}.
left(_, _, _, ExId, _, _) ->
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
