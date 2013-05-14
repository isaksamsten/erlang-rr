%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').
-define(DATE, "2013-05-14").
-define(MAJOR_VERSION, "0").
-define(MINOR_VERSION, "1").
-define(REVISION, "1.0").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").


main([Hd|Args]) ->
    Props = read_config(),
    initialize(Props),
    Cmd = list_to_atom(Hd),
    case Cmd of
	rf ->
	    rf:main(Args);
	help ->
	    show_help()	    
    end;
main([]) ->
    ok.

read_config() ->
    Default = [{log_level, info}, {log_target, std_err}],
    case file:consult("rr.config") of
	{ok, Prop} ->
	    Prop;
	{error, Reason} ->
	    io:format("Could not read rr.config: '~p'. ~n", [Reason]),
	    Default;
	{error, {Line, Mod, Term}} ->
	    io:format("Malformed config file. Error at: ~p. ~n", [{Line, Mod, Term}]),
	    Default
    end.

initialize(Props) ->
    rr_log:new(proplists:get_value(log_target, Props, std_err),
	       proplists:get_value(log_level, Props, info)),
    ok.

show_help(options, CmdSpec, Application) ->
    io:format("~s~n", [show_information()]),
    getopt:usage(CmdSpec, Application).

show_help() ->
    io:format("~s~n", [show_information()]),
    io:format("Commands:
   rf
   help
").

show_information() -> 
    io_lib:format("rr (Random Rule Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR]).
