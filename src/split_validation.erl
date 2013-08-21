%%% @author Isak Karlsson  <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%% Module for performing cross validation
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(split_validation).

-export([
	 evaluate/2
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

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
