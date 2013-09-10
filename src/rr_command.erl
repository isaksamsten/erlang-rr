%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(rr_command).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {parse_args, 1}, %% use rr:parse/2 to parse args
     {main, 1}, %% run the algorithm
     {help, 0}, %% return a string with help
     
     {args, 2}, %% get all args which are used with this command
     {args, 3}  %% get arg with key
    ];
behaviour_info(_) ->
    undefined.


