%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_evaluator).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{evaluate, 2}].

