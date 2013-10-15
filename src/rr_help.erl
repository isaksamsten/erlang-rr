%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(rr_help).

-behaviour(rr_command).
-behaviour(rr_module).

-export([
	 main/1,
	 parse_args/1,

	 help/0,
	 args/2
	]).

parse_args([]) ->
    rr_help;
parse_args([Module|_]) ->
    element(1, rr:get_module(Module)).

main(Module) ->
    Module:help(),
    ok.

args(_,_) ->
    ok.
help() ->
    rr:show_help(),
    ok.

    
    

