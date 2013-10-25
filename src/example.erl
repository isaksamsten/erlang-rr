%%% @author Isak Karlsson <isak@n183-p87.kthopen.kth.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2013 by Isak Karlsson <isak@n183-p87.kthopen.kth.se>

-module(example).
-export([
         id/1,
         count/1,
         count/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc get the id of an example
id({Id, _}) ->
    Id;
id(Id) ->
    Id.

%% @doc get the number of examples (of example)
count({_Id, Count}) ->
    Count;
count(_Id) ->
    1.

%% @doc update the count of example
count({Id, Count}, Inc) ->
    {Id, Count + Inc};
count(Id, Inc) ->
    {Id, 1+Inc}.

-ifdef(TEST).

id_test() ->
    ?assertEqual(10, id({10, 1})),
    ?assertEqual(10, id(10)).

count_test() ->
    ?assertEqual(2, count({10, 2})),
    ?assertEqual(1, count(10)).

count2_test() ->
    ?assertEqual({10, 4}, count({10, 2}, 2)),
    ?assertEqual({10, 2}, count(10, 1)).

-endif.
