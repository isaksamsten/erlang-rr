%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 17 Jan 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_processor).
-export([
         behaviour_info/1,
         find/1
        ]).

behaviour_info(callbacks) ->
    [{new, 1}].

%% @doc find a processor given a string
find(EString) ->
    case rr:get_processor(EString) of
        {Processor, Args} ->
            Opts = Processor:args(Args),
            Processor:new(Opts);
        error ->
            throw({module_not_found, EString})
    end.

