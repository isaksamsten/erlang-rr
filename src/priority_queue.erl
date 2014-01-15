-module(priority_queue).

-export([
         new/2,
         new/1,
         push/2,
         peek/1,
         to_list/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new(Comparator, K) ->
    spawn_link(fun () -> loop(Comparator, K, []) end).

new(K) ->
    new(fun(A, B) -> A < B end, K).

loop(Comparator, K, List) ->
    receive 
        {push, Ref, Self, Item} ->
            Self ! {ok, Ref},
            loop(Comparator, K, push_item(Item, Comparator, K, List));
        {peek, Ref, Me} ->
            io:format("The list: ~p~n,", [List]),
            Me ! {peek, Ref, peek_item(List)},
            loop(Comparator, K, List);
        {list, Ref, Me} ->
            Me ! {list, Ref, List},
            loop(Comparator, K, List)
    end.

push_item(Item, Comparator, K, List) ->
    push_item(Item, Comparator, K, 1, List, []).

push_item(_, _, K, Current, _, Acc) when Current > K->
    Acc;
push_item(Item, _, _, _, [], Acc) ->
    lists:reverse([Item|Acc]);
push_item(Item, Comparator, K, Current, [Head|List], Acc) ->
    case Comparator(Item, Head) of
        true ->
            %push_item(Item, Comparator, K, Current + 1, List, [Item,Head|Acc]);
            push_rest([Head,Item|Acc], K, List);
        false ->
            push_item(Item, Comparator, K, Current + 1, List, [Head|Acc])
    end.

push_rest([], K, Acc) ->
    lists:sublist(Acc, K);
push_rest([H|R], K, Acc) ->
    push_rest(R, K, [H|Acc]).

peek_item([]) ->
    undefined;
peek_item([Head|_]) ->
    Head.

push(This, Item) ->
    Ref = monitor(process, This),
    Self = self(),
    This ! {push, Ref, Self, Item},
    receive
        {ok, Ref} ->
            demonitor(Ref)
    end.

peek(This) ->
    Ref = monitor(process, This),
    Self = self(),
    This ! {peek, Ref, Self},
    receive
        {peek, Ref, Item} ->
            Item
    end.

to_list(This) ->
    Ref = monitor(process, This),
    Self = self(),
    This ! {list, Ref, Self},
    receive
        {list, Ref, List} ->
            List
    end.                      

-ifdef(TEST).
push_test() ->
    Cmp = fun(A, B) -> A < B end,
    ?assertEqual(push_item(10, Cmp, 3, []), [10]),
    ?assertEqual(push_item(9, Cmp, 3, [10]), [9, 10]),
    ?assertEqual(push_item(8, Cmp, 3, [9,10,11]), [8, 9, 10]).

test() ->
    Cmp = fun(A, B) -> A < B end,
    Queue = new(Cmp, 5),
    push(Queue, 3),
    push(Queue, 10),
    push(Queue, 1),
    push(Queue, 14),
    peek(Queue),
    ?assertEqual(1, peek(Queue)),
    ?assertEqual([1,3,10,14], to_list(Queue)).


-endif.
