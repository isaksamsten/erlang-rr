%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(help).
-behaviour(rr_command).

-export([
	 main/1,
	 parse_args/1,

	 help/0,
	 args/2,
	 args/3
	]).

parse_args([]) ->
    help;
parse_args([Module|_]) ->
    list_to_atom(Module).

main(Module) ->
    Module:help(),
    ok.

args(_,_,_) ->
    ok.
args(_,_) ->
    ok.
help() ->
    ok.

    
    

