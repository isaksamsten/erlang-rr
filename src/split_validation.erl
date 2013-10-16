%%% @author Isak Karlsson  <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%% Module for performing cross validation
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(split_validation).

-behaviour(rr_command).
-behaviour(rr_evaluator).

-export([
	 evaluate/2,

	 help/0,
	 parse_args/1,
	 args/1
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

-define(NAME, "sv").
-define(CMD_SPEC, 
	[{<<"ratio">>, $f, "ratio", {float, 0.66},
	  "Split dataset into two parts - one for building the model and one for testing it."}
	]).

help() ->
    rr:show_help(options, ?CMD_SPEC, "sv").

parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

args(Args) ->
    args(Args, fun (Value, Reason) -> throw({bad_arg, ?NAME, Value, Reason}) end).

args(Args, Error) ->
    Ratio = args(<<"ratio">>, Args, Error),
    [{ratio, Ratio}].

args(Key, Args, Error) ->
    Value = proplists:get_value(Key, Args),
    case Key of
	<<"ratio">> ->
	    Value;
	_ ->
	    Error("sv", Key)
    end.


%% @doc split examples and train and evaluate
-spec evaluate(example_set(), any()) -> result_set().
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
    Ratio = proplists:get_value(ratio, Props, 0.66),
    {Train, Test} = rr_example:split_dataset(Examples, Ratio),
    Model = Build(Features, Train, ExConf),
    Result = Evaluate(Model, Test, ExConf),
    {{split, {{ratio, Ratio}, Result}}, Model}.
