%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_command).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {parse_args, 1}, %% use rr:parse/2 to parse args
     {args, 1} %% get all args which are used with this command (throws {bad_arg, ModuleName, ArgName, Value})
    ];
behaviour_info(_) ->
    undefined.
