%%% @author Isak Karlsson <isak@Unkown-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Testing the error in sample
%%% @end
%%% Created :  5 Dec 2013 by Isak Karlsson <isak@Unkown-MacBook-Pro.local>

-module(in_sample_validation).

-export([
         evaluate/2,
         help/0,
         args/1,
         parse_args/1
        ]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

-behaviour(rr_command).
-behaviour(rr_evaluator).

-define(NAME, "iv").
-define(CMD_SPEC, []).


help() ->
    rr:show_help(options, ?CMD_SPEC, "iv").

parse_args(_Args) ->
    [].%rr:parse(?NAME, Args, ?CMD_SPEC).

args(Args) ->
    args(Args, fun (Value, Reason) -> throw({bad_arg, ?NAME, Value, Reason}) end).

args(_Args, _Error) ->
    [].

evaluate(ExSet, Props) ->
    Build = case proplists:get_value(build, Props) of
                undefined -> throw({badarg, build});
                Build0 -> Build0
            end,
    Evaluate = case proplists:get_value(evaluate, Props) of
                   undefined -> throw({badarg, evaluate});
                   Evaluate0 -> Evaluate0
               end,
    #rr_exset {
       features = Features,
       examples = Examples,
       exconf = ExConf
      } = ExSet,
    Model = Build(Features, Examples, ExConf),
    Result = Evaluate(Model, Examples, ExConf),
    {{in_sample, Result}, Model}.


