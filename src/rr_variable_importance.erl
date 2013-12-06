%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_variable_importance).
-behaviour(rr_command).
-behaviour(rr_module).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         %% rr_command
         parse_args/1,
         args/1,
         
         %% rr_module 
         main/1,
         help/0
        ]).

-include("rr.hrl").
-define(CMD_SPEC, 
        [{<<"model">>, $m, "model", string,
          "Name of the deployed model to get variable importance for."}]).

-define(NAME, "vi").

%% @doc parse the arguments
parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

%% @doc show help
help() ->
    rr:show_help(options, ?CMD_SPEC, "vi").

args(_) ->
    [].

main(Args) ->
    ModelFile = proplists:get_value(<<"model">>, Args),
    {Module, Dump} = load(ModelFile),
    {_Model, Conf} = Module:unserialize(Dump),
    VI = Conf#rr_ensemble.vi,
    lists:foreach(fun ({FeatureId,Importance}) ->
                          io:format("~p:~p~n", [FeatureId, Importance])
                  end, VI).

load(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            rr_system:unserialize_model(Binary);
        {error, Reason} ->
            {error, Reason}
    end.
    
    
    
