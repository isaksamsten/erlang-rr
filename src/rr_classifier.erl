%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(rr_classifier).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {new, 1},
     {build, 4},
     {evaluate, 4},

     %% {partial_build, 1},
     %% {partial_evaluate, 1}

     {serialize, 2},
     {unserialize, 1},
     {kill, 1}
    ].

