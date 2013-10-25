%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>

-module(rr_util).

-export([
	 weighted_random/1,
	 weighted_random/2,
	 weighted_random/3,

	 partition/2,

	 min/2,
	 max/2,
	 shuffle/1,
	 safe_iolist_to_binary/1
	]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc List should be sorted with the highest Weight first and the weights should sum to 1
-spec weighted_random([{Weight::float(), Item::any()},...]) -> Item::any().
weighted_random(List) ->
    weighted_random(List, 1).

weighted_random(List, Total) ->
    weighted_random(List, Total, fun(X) -> X end).

weighted_random(List, Total, Sort) ->
    Sorted = Sort(List),
    Rnd = random:uniform() * Total,
    weighted_random1(Sorted, Rnd).

weighted_random1([], _) ->
    error;
weighted_random1([{Weight, Item}|_], Rnd) when Rnd < Weight ->
    Item;
weighted_random1([{Weight, _}|Rest], Rnd) ->
    weighted_random1(Rest, Rnd - Weight).

safe_iolist_to_binary(List) when is_list(List) ->
    iolist_to_binary(List);
safe_iolist_to_binary(Value) ->
    Value.

%% @doc parttion elements from List evenly over each list in Parts
-spec partition(List::[], Parts::[[]]) -> [[]].
partition([], Parts) ->
    Parts;				     
partition([Model|Models], [Part|Parts]) ->
    partition(Models, Parts ++ [[Model|Part]]).

min(Fun, List) ->
    {_, Min} = lists:foldl(
		 fun (Value, {OldMin, OldValue}) ->
			 U = Fun(Value),
			 if U < OldMin ->
				 {U, Value};
			    true ->
				 {OldMin, OldValue}
			 end
		 end, {Fun(hd(List)), hd(List)}, tl(List)),
    Min.


max(Fun, List) ->
    {_, Max} = lists:foldl(fun (Value, {OldMax, OldValue}) ->
				   U = Fun(Value),
				   if U > OldMax ->
					   {U, Value};
				      true ->
					   {OldMax, OldValue}
			end
			   end, {Fun(hd(List)), hd(List)}, tl(List)),
    Max.

%% @doc randomly permute a list (public)
shuffle(List) ->
    shuffle_list(List).

%% @doc randomly permute a list of items
shuffle_list(Ids0) ->
    [Id || {_, Id} <- lists:keysort(1, lists:map(fun (Id) -> {random:uniform(), Id} end, Ids0))].

-ifdef(TEST).
weighted_list() ->
     [{0.40, a}, {0.25, b}, {0.15, c}, {0.1, d}, {0.05, e}, {0.05, f}].

test_distribution(List, Total) ->    
    Dict = lists:foldl(fun (_I, Dict) ->
			       R = weighted_random(List),
			       dict:update_counter(R, 1, Dict)
		       end, dict:new(), lists:seq(1, Total)),
    lists:map(fun ({A, X}) -> {round(X/Total * 100)/100, A} end, dict:to_list(Dict)).

distribution_test() ->
    random:seed(1,2,3),
    List = weighted_list(),
    D = test_distribution(List, 100000),
    ?assertEqual(D, List).

-endif.

