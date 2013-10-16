%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% A module is a callable component used in the command line
%%% interface.  It provides a "main"-function which accpets a list of
%%% commands in proplists format with binaries as keys. Help is a
%%% function which should print the commands for this particular
%%% module to stdout and halt execution. Main should return ok when
%%% terminating correctly or error when there is an error. In the best
%%% case - the main function should never throw an error.
%%%
%%% A module string is defined as "module [args...]", where args is a
%%% list of tokens separated by a space
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_module).
-export([
	 behaviour_info/1,
	 find/1
	]).

behaviour_info(callbacks) ->
    [
     {main, 1}, %% run the algorithm
     {help, 0}  %% return a string with help
    ];
behaviour_info(_) ->
    undefined.

%% @doc find a module defined in the config using a module string
find(MString) ->
    case rr:parse_args(MString, 'rr.modules') of
	error ->
	    throw({module_not_found, MString});
	Ow -> Ow
    end.
    
