%%% @author Isak Karlsson <isak@dhcp-158-243.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2013 by Isak Karlsson <isak@dhcp-158-243.dsv.su.se>

-module(feature).
-export([
         id/1,
         type/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc get the id of a feature
id({_Type, Id}) ->
    Id.

%% @doc get the type of a feature
type({Type, _Id}) ->
    Type.

-ifdef(TEST).

id_test() ->
    ?assertEqual(100, id({numeric, 100})).

type_test() ->
    ?assertEqual(numeric, type({numeric, 1000})).

-endif.
