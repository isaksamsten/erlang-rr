%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% A rr_classifier is an algorithm that can be used to classify
%%% examples found in a dataset loaded using rr_example:load/1. The
%%% new/1 function is supposed to return a model descriptor (which can
%%% be anything) which is used to build a model with specified
%%% parameters. The build/2 function takes as arguments the model
%%% descriptor returned by new/1 and a example set return by
%%% rr_example:load/1 form this a new model is generated and
%%% returned. The evaluate/4 takes as arguments the model descriptor,
%%% the model returned by build/2, a set of test examples and the
%%% configuration for an example set and returns a proplists with
%%% evaluation metrics.
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(rr_classifier).
-export([
	 behaviour_info/1,
	 kill/2,
	 find/1
	]).

-type classifier_key() :: build | evaluate | '$config' | '$module'.
-type classifier_attr() :: {classifier_key(), fun()}.
-type classifier_attrs() :: [classifier_attr(),...].

behaviour_info(callbacks) ->
    [
     {new, 1},

     {build, 2},
     {build, 4},

     {evaluate, 4},

     {partial_build, 1},
     {partial_evaluate, 1},

     {serialize, 2},
     {unserialize, 1},
     {kill, 1}
    ].

%% @doc find the classifier for the CString.
-spec find(string()) -> classifier_attrs().
find(CString) ->
    case rr:get_classifier(CString) of
	{Classifier, Args} ->
	    Opts = Classifier:args(Args),
	    Rf = Classifier:new(Opts),
	    Build = Classifier:partial_build(Rf),
	    Evaluate = Classifier:partial_evaluate(Rf),
	    [{build, Build}, 
	     {evaluate, kill(Classifier, Evaluate)}, 
	     {'$config', Rf}, 
	     {'$module', Classifier}];
	error ->
	    throw({module_not_found, CString})
    end.

%% @doc kill (to clean up unused models) after evaluation (to reduce
%% memory footprint during cross validation)
kill(Classifier, Evaluate) ->
    fun (Model, Test, ExConf) ->
	    Result = Evaluate(Model, Test, ExConf),
	    Classifier:kill(Model),
	    Result
    end.
