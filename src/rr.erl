%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-author('isak-kar@dsv.su.se').
-define(DATE, "2013-09-11").
-define(MAJOR_VERSION, "2").
-define(MINOR_VERSION, "0").
-define(REVISION, "0.0").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").

-export([
	 main/1,
	 parse_args/1,
	 parse_string_args/1,

	 show_help/3,
	 show_help/0,

	 parse/2,
	 warn/1,
	 warn/2,
	 illegal/1,
	 illegal/2,
	 illegal/3,
	 illegal_option/2,
	 seconds/1,
	 
	 read_config/1,
	 get_opt_name/2,
	 any_opt/2,

	 get_classifier/1,
	 get_classifier/2,

	 get_evaluator/1,
	 get_evaluator/2,

	 get_module/1
	]).

get_classifier(Value, Error) ->
    case rr:get_classifier(Value) of
	{Classifier, Args} ->
	    Opts = Classifier:args(Args, Error),
	    Rf = Classifier:new(Opts),
	    Build = Classifier:partial_build(Rf),
	    Evaluate = Classifier:partial_evaluate(Rf),
	    [{build, Build}, 
	     {evaluate, rf:killer(Evaluate)}, 
	     {'$config', Rf}, 
	     {'$module', Classifier}];
	error ->
	    Error("classifier", "unknown classifier")
    end.

get_evaluator(Value, Error) ->
    case rr:get_evaluator(Value) of
	{Evaluator, Args} ->
	    Opts = Evaluator:args(Args, Error),
	    fun (ExSet, NewOpts) ->
		    Evaluator:evaluate(ExSet, NewOpts ++ Opts)
	    end;
	error ->
	    Error("evaluator", "unknown evaluator")
    end.

get_classifier(Cstring) ->
    parse_string_args(Cstring, 'rr.classifiers').

get_evaluator(Estring) ->
    parse_string_args(Estring, 'rr.evaluators').

get_module(MString) ->    
    parse_string_args(MString, 'rr.modules').

parse_string_args(Value) ->
    parse_string_args(Value, 'rr.modules').

parse_string_args(Value, Config) ->
    parse_args((catch string:tokens(Value, " ")), Config).

parse_args(Args) ->
    parse_args(Args, 'rr.modules').

%% @doc returns the parse arguments and a suitable module
parse_args([Key|Args], Config) ->
    case lists:keyfind(Key, 1, rr_config:get_value(Config)) of
	{Key, Atom, _} ->
	    {Atom, Atom:parse_args(Args)};
	false ->
	    error
    end;
parse_args(_, _) ->
    error.

main(Args) ->
    Props = read_config("rr.config"),
    ok = initialize(Props),
    case parse_args(Args) of
	{Method, MethodArgs} ->
	    ok = Method:main(MethodArgs);
	error ->
	    io:format(standard_error, "invalid command specified~n", []),
	    show_help()
    end,
    rr_config:stop(),
    rr_log:stop().

read_config(File) ->
    case rr_config:read_config_file(File) of
	{ok, Props} ->
	    Props;
	{error, {Line, _, Term}} ->
	    io:format(standard_error, "malformed configuration file: \"~s\" (line: ~p). ~n", [Term, Line]),
	    halt();
	{error, Reason} ->
	    io:format(standard_error, "could not read 'rr.config': '~p'. ~n", [Reason]),
	    halt()
    end.

initialize(Props) ->
    rr_config:init(Props),
    rr_log:new(proplists:get_value('log.target', Props, std_err),
	       proplists:get_value('log.level', Props, info)),
    case code:add_pathz(filename:dirname(escript:script_name()) ++
			    rr_config:get_value('rr.plugin.dir')) of
	{error, _} ->
	    rr_log:debug("plugin dir does not exist");
	_ -> true
    end,
    ok.

show_help(options, CmdSpec, Application) ->
    io:format(standard_error, "~s~n", [show_information()]),
    getopt:usage(CmdSpec, "rr " ++ Application),
    halt(2).

show_help() ->
    io:format(standard_error, "~s~n", [show_information()]),
    io:format(standard_error, "Commands:~n", []),
    Modules = rr_config:get_value('rr.modules'),
    Sorted = lists:sort(fun({A,_,_}, {B,_,_}) -> length(A) > length(B) end, Modules),
    Longest = length(element(1, hd(Sorted))),
    lists:foreach(
      fun({_, _, undefined}) ->
	      ok;
	 ({Module, _, Desc}) ->
	      io:format(standard_error, "  ~s    ~s~n", 
			[string:left(Module, length(Module) + Longest - length(Module)), Desc])
      end, Modules),
    halt(2).
					 

show_information() -> 
    io_lib:format("rr (Random Rule Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR]).


%% configuration helpers
get_opt_name(Name, []) ->
    Name;
get_opt_name(Name, [{RealName, _, Long, _Default, _Descr}|Rest]) ->
    if Name == RealName ->
	    Long;
       true ->
	    get_opt_name(Name, Rest)
    end.
    
any_opt([], _) ->
    false;
any_opt([O|Rest], Options) ->
    case lists:member(O, Options) of
	true ->
	    O;
	false ->
	    any_opt(Rest, Options)
    end.

warn(String) ->
    io:format(standard_error, ["warn: "|String], []).
warn(String, Args) ->
    io:format(standard_error, ["warn: "|String], Args).

%% error reporting
illegal(Argument, Error) ->
    illegal(Argument, Error, []),
    halt().

illegal(Argument, Error, Args) ->
    io:format(standard_error, "rr: for argument '~s', ~s. ~n", [Argument, io_lib:format(Error, Args)]),
    halt().

illegal_option(Argument, Option) ->
    illegal(io_lib:format("unrecognized option '~s' for '~s'", [Option, Argument])).

illegal(Error) ->
    io:format(standard_error, "rr: ~s. ~nPlease consult the manual.~n", [Error]),
    halt(2).

%% @doc calculates the number of seconds between now() and Time
seconds(Time) ->
    timer:now_diff(erlang:now(), Time)/1000000.

parse(Args, Options) ->
    case getopt:parse(Options, Args) of
	{ok, {Parsed, _}} -> 
	    Parsed;
	{error, {invalid_option, R}} ->
	    rr:illegal(io_lib:format("unrecognized option '~s'", [R]));
	{error, {missing_option_arg, R}} ->
	    rr:illegal(io_lib:format("missing argument to option '~s'", [rr:get_opt_name(R, Options)]));
	{error, _} ->
	    rr:illegal("unknown error")
    end.
