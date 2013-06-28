%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%% Small named module (process) for handling global configuration options
%%% @end
%%% Created : 21 May 2013 by  <Isak@ISAK-PC>
-module(rr_config).
-export([
	 init/1,
	 stop/0,
	 loop/1,

	 get_value/1,
	 get_value/2
	]).

init(Props) ->
    Pid = spawn_link(?MODULE, loop, [Props]),
    register(config, Pid).

stop() ->
    Ref = monitor(process, config),
    config ! {exit, Ref, self()},
    receive 
	{ok, Ref} ->
	    demonitor(Ref),
	    ok;
	_ ->
	    error
    end.

get_value(Prop) ->
    Ref = monitor(process, config),
    config ! {get, Prop, self(), Ref},
    receive
	{get, Ref, Value} ->
	    demonitor(Ref),
	    Value;
	_ ->
	    undefined
    end.

get_value(Prop, Default) ->
    case get_value(Prop) of
	undefined ->
	    Default;
	X -> X
    end.

loop(Props) ->
    receive
	{get, Prop, Pid, Ref} ->
	    Value = proplists:get_value(Prop, Props),
	    Pid ! {get, Ref, Value},
	    loop(Props);
	{exit, Ref, Pid} ->
	    Pid ! {ok, Ref};
	_ ->
	    loop(Props)
    end.



