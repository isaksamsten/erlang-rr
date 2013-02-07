-module(rr_id).
-compile(export_all).

new(Item) ->
    sets:from_list([Item]).

add(Item, Ids) ->
    sets:add_element(Item, Ids).

subtract(Ids0, Ids) ->
    sets:subtract(Ids0, Ids).

union(A, B) ->
    sets:union(A, B).

fold(Fun, Ids) ->
    sets:fold(Fun, Ids).
