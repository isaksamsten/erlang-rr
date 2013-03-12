%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Functions for handling missing values
%%% @end
%%% Created : 13 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_missing).
-compile(export_all).

random(_, _, _, _) ->
    random:uniform() =< 0.5.

biased(_, _, {_, NoLeft, _}, {_, NoRight, _}) ->
    LeftFraction = (NoLeft + 1) / (NoLeft + NoRight + 2),
    random:uniform() =< LeftFraction.

right(_, _, _, _) ->
    false.

