%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(bootstrap_validation).

-export([
	 evaluate/4
	]).

%% @headerfile "rr.hrl"
-include("rr.hrl").

%% @doc evaluate a model on a bootstrap of examples
-spec evaluate(features(), examples(), #rr_example{}, any()) -> result_set().
evaluate(Features, Examples, ExConf, Props) ->
    Build = case proplists:get_value(build, Props) of
		undefined ->
		    throw({badarg, build});
		BuildFun -> BuildFun
	    end,
    Evaluate = case proplists:get_value(evaluate, Props) of
		   undefined -> throw({badarg, evaluate});
		   EvaluateFun -> EvaluateFun
	       end,
    {Train, Test} = rr_example:bootstrap_aggregate(Examples),
    Model = Build(Features, Train, ExConf),
    Result = Evaluate(Model, Test, ExConf),
    {{bootstrap, {{in_bag_fraction, rr_example:count(Train)/rr_example:count(Test)}, Result}}, Model}.

