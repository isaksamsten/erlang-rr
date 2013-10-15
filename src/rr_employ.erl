%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_employ).
-behaviour(rr_command).
-behaviour(rr_module).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("rr.hrl").

-export([
	 parse_args/1,
	 args/2
	]).

-export([
	 main/1,
	 help/0
	]).

-define(CMD_SPEC, 
	[{<<"dataset">>, $i, "input", string,
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."},
	 {<<"model">>, $m, "model", string,
	    "Name of the deployed model to employ."}]).

%% @doc parse the arguments
parse_args(Args) ->
    rr:parse(Args, ?CMD_SPEC).

%% @doc show help
help() ->
    rr:show_help(options, ?CMD_SPEC, "employ").

args(_, _) ->
    [].

main(Args) ->
    Dataset = proplists:get_value(<<"dataset">>, Args),
    ModelFile = proplists:get_value(<<"model">>, Args),
    {Module, Dump} = load(ModelFile),
    {Model, Conf} = Module:load(Dump),
    Cores = erlang:system_info(schedulers),
    rr_log:info("loading '~s' on ~p core(s)", [Dataset, Cores]),
    Csv = csv:binary_reader(Dataset),
    ExSet = rr_example:load(Csv, Cores),
    Res = Module:evaluate(Conf, Model, ExSet#rr_exset.examples, ExSet#rr_exset.exconf),
    Output = rr_result:default(),
    Output(Res),
    ok.
    
load(File) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    rr_system:unserialize_model(Binary);
	{error, Reason} ->
	    {error, Reason}
    end.		
