%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_deploy).
-behaviour(rr_command).
-behaviour(rr_module).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("rr.hrl").

-export([
	 parse_args/1,
	 args/1
	]).

-export([
	 main/1,
	 help/0
	]).

-define(CMD_SPEC, 
	[{<<"classifier">>, $c, "classifier", {string, "rf -n 10"},
	  "Choose the classification method"},
	 {<<"dataset">>, $i, "input", string,
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."},
	 {<<"output">>, $o, "output", string,
	    "Name for the deployed model."}]).

-define(NAME, "deploy").

%% @doc parse the arguments
parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

%% @doc show help
help() ->
    rr:show_help(options, ?CMD_SPEC, "deploy").

args(_) ->
    [].

main(Args) ->
    Classifier = rr_classifier:find(proplists:get_value(<<"classifier">>, Args)),
    Dataset = proplists:get_value(<<"dataset">>, Args),
    Output = proplists:get_value(<<"output">>, Args),
    Cores = erlang:system_info(schedulers),
    rr_log:info("loading '~s' on ~p core(s)", [Dataset, Cores]),
    Csv = csv:binary_reader(Dataset),
    ExSet = rr_example:load(Csv, Cores),
    Build = proplists:get_value(build, Classifier),
    Config = proplists:get_value('$config', Classifier),
    Module = proplists:get_value('$module', Classifier),
    Model = Build(ExSet#rr_exset.features, ExSet#rr_exset.examples, ExSet#rr_exset.exconf),
    Data = Module:serialize(Config, Model),
    file:write_file(Output, Data),
    rr_log:info("deployed model to '~s'", [Output]),
    rr_log:stop(),
    ok.
    
    
    
    
