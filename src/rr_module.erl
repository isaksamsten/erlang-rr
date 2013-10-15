%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_module).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {main, 1}, %% run the algorithm
     {help, 0}  %% return a string with help
    ];
behaviour_info(_) ->
    undefined.


