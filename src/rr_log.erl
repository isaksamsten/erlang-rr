%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  5 Mar 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_log).
-author('isak-kar@dsv.su.se').
-export([
	 info/1,
	 info/2,
	 
	 debug/1,
	 debug/2,

	 log/3,
	 log/2,
	 new/2,
	 loop/2,
	 stop/0,
	 to_number/1
	]).

new(std_err, Max) ->
    Pid = spawn_link(fun() -> loop(std_err, to_number(Max)) end),
    register(log, Pid);
new(File, Max) ->
    case file:open(File, [write]) of
	{ok, IoDevice} ->
	    Pid = spawn_link(fun() -> loop(IoDevice, to_number(Max)) end),
	    register(log, Pid);
	{error, _Reason} ->
	    new(std_err, to_number(Max))
    end.

stop() ->
    log ! {stop, self()},
    receive
	ok ->
	    ok
    end.

info(Str, Params) ->
    log(info, Str, Params).
info(Str) ->
    log(info, Str).

debug(Str, Params) ->
    log(debug, Str, Params).
debug(Str) ->
    log(debug, Str).

log(Level, Str, Params) ->
    try
	log ! {log, Level, Str, Params}
    catch 
	_:_ ->
	    ok
    end.
log(Level, Str) ->
    log(Level, Str, []).

loop(Device, Max) ->
    receive
	{log, Level, Str, Params} ->
	    do_log(Device, to_number(Level), Max, atom_to_list(Level) ++ ": " ++ io_lib:format(Str, Params)),
	    loop(Device, Max);
	{stop, Pid} ->
	    Pid ! ok
    end.

do_log(_, Level, Max, _) when Level > Max ->
    ok;
do_log(std_err, _Level, _Max, Str) ->
    io:format(standard_error, [Str|"\n"], []);
do_log(Device, _Level, _Max, Str) ->
    file:write(Device, [Str|"\n"]).

to_number(Level) ->
    case Level of
	none -> 0;
	error -> 1;
	info  -> 2;
	debug -> 3;
	_ -> 0
    end.

