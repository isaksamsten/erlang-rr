%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-author('isak-kar@dsv.su.se').
-define(DATE, "2013-10-16").
-define(MAJOR_VERSION, "3").
-define(MINOR_VERSION, "0").
-define(REVISION, "0.0").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").

-export([
         main/1,
         parse_args/1,
         parse_args/2,
         parse_string_args/1,
         parse_string_args/2,
         parse_option_string/1,
         show_help/3,
         show_help/0,
         all_modules/0,

         parse/3,
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
         get_evaluator/1,
         get_module/1
        ]).

%% @doc get classifier from the available classifiers using a CString
%% (e.g. "rf -n 100") would return {rf, [....]} @end
get_classifier(Cstring) ->
    parse_string_args(Cstring, 'rr.classifiers').

%% @doc get evaluator from the available evaluator
get_evaluator(Estring) ->
    parse_string_args(Estring, 'rr.evaluators').

%% @doc get evaluator from the available modules
get_module(MString) ->    
    parse_string_args(MString, 'rr.modules').

%% @doc legacy
%% @deprecated
parse_string_args(Value) ->
    parse_string_args(Value, 'rr.modules').

%% @doc get the module and its (parsed) arguments
-spec parse_string_args(string(), atom()) -> {atom(), [{any(), any()},...]}.
parse_string_args(Value, Config) ->
    parse_args((catch string:tokens(string:strip(Value), " ")), Config).

%% @doc legacy
%% @deprecated
parse_args(Args) ->
    parse_args(Args, 'rr.modules').

%% @doc parse an option string (i.e. a string with comma or space
%% separated numbers) Example: "1,2,3" would return [1,2,3] @end
parse_option_string(Option) ->
    lists:map(fun (X) -> 
                      element(2, rr_example:format_number(X)) 
              end, string:tokens(Option, ", ")).

%% @doc returns the parse arguments and a suitable module
-spec parse_args([Command::string(), ...], atom()) -> {atom(), [{any(), any()}, ...]}.
parse_args([Key|Args], Config) ->
    case lists:keyfind(Key, 1, rr_config:get_value(Config)) of
        {Key, Atom, _} ->
            {Atom, Atom:parse_args(Args)};
        false ->
            error
    end;
parse_args(_, _) ->
    error.

%% @doc run the specified modules
main(Args) ->
    Props = read_config("rr.config"),
    ok = initialize(Props),
    case rr_module:find(Args) of
        {Method, MethodArgs} ->
            Seed = rr_config:get_value('random.seed', {100,100,100}),
            random:seed(Seed),
            case execute(Method, MethodArgs) of
                ok ->
                    halt(0);
                {error, _Reason} ->
                    halt(1)
            end;
        error ->
            io:format(standard_error, "invalid command specified~n", []),
            show_help()
    end,
    rr_config:stop(),
    rr_log:stop().

%% @doc execute the selected module
execute(Method, MethodArgs) ->
    try
        Method:main(MethodArgs)
    catch
        throw:{bad_arg, Where, Arg, Value} ->
            io:format(standard_error, "rr: invalid value ('~s') to argument ('~s') in command '~s'~n", [Value, Arg, Where]),
            {error, bad_arg};
        throw:{invalid_option, Where, R} ->
            rr:illegal(io_lib:format("unrecognized option '~s' in command '~s'", [R, Where])),
            {error, bad_arg};
        throw:{missing_option_arg, Where, R} ->
            rr:illegal(io_lib:format("missing argument to option '~s' in command '~s'", [R, Where])),
            {error, bad_arg};
        throw:{unkown_arg_error, Where} ->
            rr:illegal(io_lib:format("unknown error in command '~s'", [Where]));
        throw:{module_not_found, _MString} ->
            rr:illegal("could not find the specified module"),
            {error, bad_module}
            %% todo: catch these?
    end.

%% @doc read a configuration file
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

%% @doc initialize configurations (for rr_config) and add the
%% plugin-dir to the loadpath @end
initialize(Props) ->
    rr_config:init(Props),
    rr_log:new(proplists:get_value('log.target', Props, std_err),
               proplists:get_value('log.level', Props, info)),
    case code:add_pathz(filename:dirname(escript:script_name()) ++
                            rr_config:get_value('rr.plugin.dir')) of
        {error, _} ->
            %rr_log:debug("plugin dir does not exist");
            error;
        _ -> true
    end,
    ok.

%% @doc show help
show_help(options, CmdSpec, Application) ->
    io:format(standard_error, "~s~n", [show_information()]),
    getopt:usage(CmdSpec, "rr " ++ Application).
    %halt(0).

%% @private show help for modules
show_help() ->
    io:format(standard_error, "~s~n", [show_information()]),
    io:format(standard_error, "Global commands:~n", []),
    Longest = find_longest_module_name(all_modules()),
    write_help_for_modules(1, Longest, rr_config:get_value('rr.modules')),

    io:format(standard_error, "~nLocal commands:~n", []),
    io:format(standard_error, "  classifiers:~n", []),
    write_help_for_modules(3, Longest-2, rr_config:get_value('rr.classifiers')),

    io:format(standard_error, "~n  evaluators:~n", []),
    write_help_for_modules(3, Longest-2, rr_config:get_value('rr.evaluators')).
    %halt(0).

all_modules() ->
    rr_config:get_value('rr.modules') ++ 
        rr_config:get_value('rr.classifiers') ++ 
        rr_config:get_value('rr.evaluators').

find_longest_module_name(Modules) ->
    Sorted = lists:sort(fun({A,_,_}, {B,_,_}) -> length(A) > length(B) end, Modules),
    length(element(1, hd(Sorted))).

write_help_for_modules(Pad, Longest, Modules) ->
    lists:foreach(
      fun({_, _, undefined}) ->
              ok;
         ({Module, _, Desc}) ->
              io:format(standard_error, string:right("~s    ~s~n", 11+Pad), 
                        [string:left(Module, length(Module) + Longest - length(Module)), Desc])
      end, Modules).
                                         
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
    io:format(standard_error, "rr: ~s. ~n", [Error]),
    halt(2).

%% @doc calculates the number of seconds between now() and Time
seconds(Time) ->
    timer:now_diff(erlang:now(), Time)/1000000.

parse(Command, Args, Options) ->
    case getopt:parse(Options, Args) of
        {ok, {Parsed, _}} -> 
            Parsed;
        {error, {invalid_option, R}} ->
            throw({invalid_option, Command, R});
        {error, {missing_option_arg, R}} ->
            throw({missing_option_arg, Command, R});
        {error, _} ->
            throw({unkown_arg_error, Command})
    end.
