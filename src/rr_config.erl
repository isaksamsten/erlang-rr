%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%% Small named module (process) for handling global configuration options
%%% @end
%%% Created : 21 May 2013 by  <Isak@ISAK-PC>
-module(rr_config).
-export([
         init/1,
         init/2,
         stop/0,
         loop/3,

         read_config_file/1,

         get_value/1,
         get_value/2
        ]).
-include_lib("kernel/include/file.hrl").

read_config_file(File) ->
    case file:consult(File) of
        {ok, Prop} ->
            {ok, Prop};
        Error -> 
            Error
    end.

init(Props) when is_list(Props) ->
    Pid = spawn_link(?MODULE, loop, [undefined, [], Props]),
    register(config, Pid),
    ok.

%% @doc if initialized with a file, the config will be reloaded if the config is updated
init(File, _Props) ->
    case read_config_file(File) of
        {ok, Value} ->
            Pid = spawn_link(?MODULE, loop, [File, last_modified(File), Value]),
            register(config, Pid),
            ok;
        Error ->
            throw(Error)
    end.
    

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
            Value
    end.

get_value(Prop, Default) ->
    case get_value(Prop) of
        undefined ->
            Default;
        X -> X
    end.

loop(File, Cfg, Props) ->
    receive
        {get, Prop, Pid, Ref} ->
            NewProps = reload_props(File, Cfg, Props),
            Value = proplists:get_value(Prop, NewProps),
            Pid ! {get, Ref, Value},
            loop(File, Cfg, NewProps);
        {exit, Ref, Pid} ->
            Pid ! {ok, Ref};
        _ ->
            loop(File, Cfg, Props)
    end.

reload_props(undefined, _, P) ->
    P;
reload_props(File, LastModified, Props) ->
    case compare_time(LastModified, last_modified(File)) of
        true ->
            reload_config_file(File, Props);
        false ->
            Props
    end.

last_modified(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime=Modified}} ->
            Modified;
        _ ->
            throw(error)
    end.

compare_time(A, B) ->
    As = calendar:datetime_to_gregorian_seconds(A),
    Bs = calendar:datetime_to_gregorian_seconds(B),
    As < Bs.

reload_config_file(File, Old) ->
    case read_config_file(File) of
        {ok, Props} ->
            Props;
        {error, {Line, _, Term}} ->
            rr_log:info("malformed config: \"~s\" (line: ~p)", [Term, Line]),
            Old;        
        _ ->
            rr_log:info("could not reload config file"),
            Old
    end.
            
    
                    
        



