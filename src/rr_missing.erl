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
    {random:uniform() =< 0.5, ExId}.

weighted_partition(build, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    partition(ExId, LeftFraction);
weighted_partition(predict, _, ExId, NoLeft, NoRight) -> 
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {random:uniform() =< LeftFraction, ExId}.
    

%%
%% Distribute the examples evenly over the left and right side
%%
random(_, _, ExId, _, _) ->
    {random:uniform() =< 0.5, {rr_example:exid(ExId), rr_example:count(ExId)}}.

%%
%% Distribute examples based on the number of examples falling in each
%% branch.
%%
weighted(_, _, ExId, NoLeft, NoRight) ->
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    {random:uniform() =< LeftFraction, {rr_example:exid(ExId), rr_example:count(ExId)}}.

ignore(_, _, _, _, _) ->
    ignore.

%%
%% Distribute every example in the right branch (i.e. consider it
%% false)
%%
right(_, _, ExId, _, _) ->
    {false, {rr_example:exid(ExId), rr_example:count(ExId)}}.
