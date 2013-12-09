%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_command).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     %%
     %% Parse args
     %% 
     %% @return [{<<"key">>, value},....]
     {parse_args, 1},

     %%
     %% get all args which are used with this command (throws
     %% {bad_arg, ModuleName, ArgName, Value})
     %%
     %% @return [{key, Value}, ....] from parse_args/1
     {args, 1}, 

     %%
     %% print usage information
     %% 
     {help, 0}
    ];
behaviour_info(_) ->
    undefined.
