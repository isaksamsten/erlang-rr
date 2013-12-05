%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_predict).
-behaviour(rr_command).
-behaviour(rr_module).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("rr.hrl").

-export([
         %% rr_command
         parse_args/1,
         args/1,
         
         %% rr_module 
         main/1,
         help/0
        ]).

-define(CMD_SPEC, 
        [{<<"example">>, $e, "example", string,
          "Specify an example as a comma separated string (only work for numerics)"},
         {<<"model">>, $m, "model", string,
            "Name of the deployed model to employ."}]).

-define(NAME, "employ").

%% @doc parse the arguments
parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

%% @doc show help
help() ->
    rr:show_help(options, ?CMD_SPEC, "employ").

args(_) ->
    [].

main(Args) ->
    ExConf = format_example(proplists:get_value(<<"example">>, Args)),
    ModelFile = proplists:get_value(<<"model">>, Args),
    {Module, Dump} = load(ModelFile),
    {Model, Conf} = Module:unserialize(Dump),

    {{Pred, _, _}, _}  = Module:predict(Conf, Model, 1, ExConf),
    io:format("~p", [list_to_integer(atom_to_list(Pred))]),
    ok.

load(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            rr_system:unserialize_model(Binary);
        {error, Reason} ->
            {error, Reason}
    end.      
    
format_example(String) ->
    ExConf = rr_example:new(),
    Items = lists:map(
              fun (Str) -> 
                      {_, Num} = rr_example:format_number(string:strip(Str)),
                      Num
              end, string:tokens(String, ",")),
    ExTable = ExConf#rr_example.examples,
    ets:insert(ExTable, list_to_tuple([1|Items])),
    ExConf.
    
    
    
