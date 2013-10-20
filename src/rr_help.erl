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
	 args/1
	]).

get_help(Module) ->
    get_help(Module, rr:all_modules()).

get_help(_, []) ->
    rr_help;
get_help(Name, [{Name, Module, _}|_]) ->
    Module;
get_help(Module, [_|Rest]) ->
    get_help(Module, Rest).

parse_args([]) ->
    rr_help;
parse_args([Module|_]) ->
    get_help(Module).

main(Module) ->
    Module:help(),
    ok.

args(_) ->
    [].

help() ->
    rr:show_help(),
    ok.
