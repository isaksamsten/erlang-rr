%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Functions for handling missing values
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_missing).
-compile(export_all).

%%
%% Distribute the examples evenly over the left and right side
%%
random(_, _, _, _, _) ->
    random:uniform() =< 0.5.

%%
%% Distribute examples based on the number of examples falling in each
%% branch.
%%
biased(Type, _, _, NoLeft, NoRight) ->
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    random:uniform() =< LeftFraction.

%%
%% Distribute every example in the right branch (i.e. consider it
%% false)
%%
right(_, _, _, _, _) ->
    false.
