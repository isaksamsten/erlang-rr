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
    case Hd of
	"rf" ->
	    rf:main(Args);
	"config" ->
	    case Args of
		["get"|Var] ->
		    io:format("~s ~n", [proplists:get_value(list_to_atom(hd(Var)), Props)]);
		["set"|_Var] ->
		    io:format("unimplemented ~n");
		_Other ->
		    io:format("invalid argument~n")
	    end;		
	_ ->
	    show_help()
    end;
main([]) ->
    io:format("no command specified~n"),
    show_help().

read_config() ->
    Default = [{log_level, info}, {log_target, std_err}],
    case file:consult("rr.config") of
	{ok, Prop} ->
	    Prop;
	{error, {Line, _, Term}} ->
	    io:format("malformed configuration file: \"~s\" (line: ~p). ~n", [Term, Line]),
	    Default;
	{error, Reason} ->
	    io:format("could not read 'rr.config': '~p'. ~n", [Reason]),
	    Default
    end.

initialize(Props) ->
    rr_log:new(proplists:get_value('log.target', Props, std_err),
	       proplists:get_value('log.level', Props, info)),
    ok.

show_help(options, CmdSpec, Application) ->
    io:format("~s~n", [show_information()]),
    getopt:usage(CmdSpec, Application).

show_help() ->
    io:format("~s~n", [show_information()]),
    io:format("Commands:
   rf             generate a random forest
   config         set and get global configuration options
   help           show program options
").

show_information() -> 
    io_lib:format("rr (Random Rule Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR]).
