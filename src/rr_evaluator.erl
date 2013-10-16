%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_evaluator).
-export([
	 behaviour_info/1,
	 find/1
	]).

-type evaluator_attrs() :: fun().

behaviour_info(callbacks) ->
    [{evaluate, 2}].

%% @doc find an evaluator given a string
-spec find(string()) -> evaluator_attrs().
find(EString) ->
    case rr:get_evaluator(EString) of
	{Evaluator, Args} ->
	    Opts = Evaluator:args(Args),
	    fun (ExSet, NewOpts) ->
		    Evaluator:evaluate(ExSet, NewOpts ++ Opts)
	    end;
	error ->
	    throw({module_not_found, EString})
    end.

