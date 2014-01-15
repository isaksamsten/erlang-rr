-module(kd_tree).
-compile(export_all).

-include("rr.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(kd_node, {left, right, axis, value, example}).

fit(Examples, Features, ExSet) ->
    Dim = list_to_tuple([F || {F, _} <- Features]),
    Flatten = rr_example:flatten(Examples),
    {Dim, kd_tree(Flatten, ExSet, Dim, 0)}.

min_max(Examples, ExSet, Dim) ->
    min_max(Examples, ExSet, Dim, 1, size(Dim), dict:new()).

min_max(_, _, _, Current, DimSize, Acc) when Current > DimSize->
    Acc;
min_max(Examples, ExSet, Dim, Current, DimSize, Acc) ->
    case element(Current, Dim) of
        categoric ->
            min_max(Examples, ExSet, Dim, Current + 1, DimSize, Acc);
        numeric ->
            MinMax = min_max(Examples, ExSet, Current, {inf, inf}),
            min_max(Examples, ExSet, Dim, Current + 1, DimSize,
                      dict:store(Current, MinMax, Acc))
    end.

min_max([], _Database, _Id, MinMax) ->
    MinMax;
min_max([ExId|Rest], ExSet, Id, {Min, Max}) ->
    Value =  rr_example:feature(ExSet, ExId, Id),
    NewMin = if Min == inf andalso Value =/= '?' ->
                     Value;
                Value == '?' ->
                     Min;
                Value < Min ->
                     Value;
                true -> 
                     Min
             end,
    NewMax = if Max == inf andalso Value =/= '?' ->
                     Value;
                Value == '?' ->
                     Max;
                Value > Max ->
                     Value;
                true ->
                     Max
             end,
    min_max(Rest, ExSet, Id, {NewMin, NewMax}).

%% @doc linear knearest neigbours (slow, use with caution)
knearest(linear, Features, Examples, Example, ExSet, K) ->
    Flatten = rr_example:flatten(Examples),
    Dim = list_to_tuple([F || {F, _} <- Features]),
    MinMax = min_max(Flatten, ExSet, Dim),
    AllDistances = lists:foldl(fun (ExId, Acc) ->
                                       ExId2 =  rr_example:exid(Example),
                                       if ExId == ExId2 ->
                                               Acc;
                                          true ->
                                               [{ExId, distance(ExId2, ExId, Dim, ExSet, MinMax)}|Acc]
                                       end
                               end, [], Flatten),    
    lists:sublist(lists:keysort(2, AllDistances), K).
    

%% @doc Target is an id
knearest({Dim, KdTree}, Example, ExSet, Ranges, K) ->
    Heap = priority_queue:new(fun ({_, A}, {_, B}) -> A < B end, K),
    knearest(Example, KdTree, Dim, K, ExSet, Ranges, 0.0, Heap),
    priority_queue:to_list(Heap).

knearest(Target, KdTree, Dim, K, ExSet, Ranges, DistanceToParent, Heap) ->
    #kd_node{left = Left, right = Right, axis = Axis, value = B, example = ExampleB} = KdTree,
    AxisType = element(Axis + 1, Dim),
    if (Left == leaf) and (Right == leaf) -> %% we are at a leaf
            if Target =/= ExampleB ->
                    Distance = distance(Target, ExampleB, Dim, ExSet, Ranges),
                    priority_queue:push(Heap, {ExampleB, Distance});
               true ->
                    ok
            end,
            undefined;
       true ->
            A =  rr_example:feature(ExSet, Target, Axis + 1),
            case less_than(AxisType, A, B) of
                true ->
                    knearest(Target, Left, Dim, K, ExSet, Ranges, DistanceToParent, Heap),
                    case priority_queue:length(Heap) of
                        Size when Size < K ->
                            knearest(Target, Right, Dim, K, ExSet, Ranges, DistanceToParent, Heap);
                        _ ->
                            {_, Best}  = priority_queue:peek(Heap),
                            Diff = sqd_point(AxisType, Axis + 1, A, B, Ranges),
                            if Diff < Best ->
                                    io:format("here"),
                                    knearest(Target, Right, Dim, K, ExSet, Ranges, DistanceToParent, Heap);
                               true -> undefined
                            end
                    end;
                false ->
                    knearest(Target, Right, Dim, K, ExSet, Ranges, DistanceToParent, Heap),
                    case priority_queue:length(Heap) of
                        Size when Size < K ->
                            knearest(Target, Left, Dim, K, ExSet, Ranges, DistanceToParent, Heap);
                        _ ->
                            {_, Best}  = priority_queue:peek(Heap),
                            Diff = sqd_point(AxisType, Axis + 1, A, B, Ranges),
                            if Diff =< Best ->
                                    knearest(Target, Left, Dim, K, ExSet, Ranges, DistanceToParent, Heap);
                               true -> undefined
                            end
                    end
            end
    end.
sqd_point(categoric, _Axis, A, B, _) ->
    normalized_diff(categoric, 0, A, B, []);
sqd_point(numeric, Axis, A, B, Ranges) ->
    normalized_diff(numeric, Axis, A, B, Ranges).

distance(ExampleA, ExampleB, Dim, ExSet, MinMax) ->
    distance(ExampleA, ExampleB, 1, size(Dim), Dim, ExSet, MinMax, 0).

distance(_, _, Current, End, _, _, _, Acc) when Current > End->
    math:sqrt(Acc);
distance(ExampleA, ExampleB, Current, DimSize, Dim, ExSet, Ranges, Acc) ->
    A = rr_example:feature(ExSet, ExampleA, Current),
    B = rr_example:feature(ExSet, ExampleB, Current),
    distance(ExampleA, ExampleB, Current + 1, DimSize, Dim, ExSet, Ranges,
                     Acc + math:pow(normalized_diff(element(Current, Dim), Current, A, B, Ranges),2)).

normalized_diff(numeric, Current, A, B, Ranges) ->
    Range = dict:fetch(Current, Ranges),
    numeric_diff(A, B, Range);
normalized_diff(categoric, _, A, B, _) ->
    categoric_diff(A, B).

categoric_diff('?', '?') ->
    1;
categoric_diff(A, B) when A == B ->
    0;
categoric_diff(_, _) ->
    1.

numeric_diff('?', '?', _) ->
    1;
numeric_diff('?', B, {Min, Max}) ->
    (B - Min)/(Max-Min);
numeric_diff(A, '?', {Min, Max}) ->
    (A - Min)/(Max-Min);
numeric_diff(A, B, {Min, Max}) ->
    Width = Max - Min,
    ((A-Min)/Width) - ((B-Min)/Width).

kd_tree([Id], _ExSet, Dim, Depth) ->
    Axis = Depth rem size(Dim),
    #kd_node{left = leaf, right = leaf, axis = Axis, example = Id};
kd_tree(Examples, ExSet, Dim, Depth) ->
    io:format("~w ~n", [Examples]),
    Axis = Depth rem size(Dim),
    AxisType = element(Axis + 1, Dim),
    Sorted = sort_axis(Axis, AxisType, ExSet, Examples),
    {Left, Median, Right} = split_at_median(Sorted, ExSet, Axis, AxisType, length(Examples) div 2),
    #kd_node {
       left = kd_tree(Left, ExSet, Dim, Depth + 1),
       right = kd_tree(Right, ExSet, Dim, Depth + 1),
       axis = Axis,
       value = Median
      }.

sort_axis(Axis, Type, ExSet, Examples) ->
    lists:sort(fun (ExampleA, ExampleB) ->
                       less_than(Type, rr_example:feature(ExSet, ExampleA, Axis + 1), 
                                 rr_example:feature(ExSet, ExampleB, Axis + 1))
               end, Examples).

less_than(_, '?', '?') ->
    false;
less_than(_, '?', _) ->
    false;
less_than(_, _, '?') ->
    true;
less_than(numeric, A, B) ->
    A < B;
less_than(categoric, A, B) ->
    A < B.

split_at_median([A,B], ExSet, Axis, numeric, _) ->
    ValueA = rr_example:feature(ExSet, A, Axis + 1),
    ValueB = rr_example:feature(ExSet, B, Axis + 1),
    {[A], (ValueA+ValueB)/2, [B]};
split_at_median(Examples, ExSet, Axis, AxisType, Median) ->
    split_at_median(Examples, 1, Median, ExSet, Axis, AxisType, []).

split_at_median([Median|Right], Current, Middle, ExSet, Axis, _, Left) when Current > Middle ->
    Value = rr_example:feature(ExSet, Median, Axis + 1),
    {Left, Value, Right};
split_at_median([Example|Rest], Current, Middle, ExSet, Axis, At, Left) ->
    split_at_median(Rest, Current + 1, Middle, ExSet, Axis, At, [Example|Left]).

testknn() ->
    File = csv:binary_reader("../data/iris.txt"),
    #rr_exset {
       features=Features, 
       examples=Examples, 
       exconf=Dataset
      } = rr_example:load(File, 4),
    {Dim, _} = Tree= fit(Examples, Features, Dataset),
    io:format("~p ~n", [Tree]),
    Flatten = rr_example:flatten(Examples),
    Ranges = min_max(Flatten, Dataset, Dim),
    {Time, Rest} = timer:tc(fun () -> knearest(Tree, 10, Dataset, Ranges, 2) end),
    {Time0, Rest0} = timer:tc(fun () -> knearest(linear, Features, Examples, 10, Dataset, 10) end),
    
    io:format("~p ~p ~n", [Time, lists:keysort(2, Rest)]),
    io:format("~p ~p ~n", [Time0, lists:keysort(2, Rest0)]).

-ifdef(TEST).
real_kd_tree_test() ->
   File = csv:binary_reader("../data/iris.txt"),
   #rr_exset {
      features=Features, 
      examples=Examples, 
      exconf=Dataset
     } = rr_example:load(File, 1),
    {Dim, _} = Tree= fit(Examples, Features, Dataset),
    Flatten = rr_example:flatten(Examples),
    Ranges = min_max(Flatten, Dataset, Dim),
    Closest = knearest(Tree, 10, Dataset, Ranges, 10),
    ?debugFmt("~p: ~p ~n", [length(Closest), lists:keysort(2, Closest)]).

min_max_test() ->
    File = csv:binary_reader("../data/iris.txt"),
    #rr_exset {
       features=Features, 
       examples=Examples, 
       exconf=Dataset
      } = rr_example:load(File, 1),
    Dim = list_to_tuple([F || {F, _} <- Features]),
    Flatten = rr_example:flatten(Examples),
    MinMax = min_max(Flatten, Dataset, Dim),
    ?debugFmt("~p ~n", [dict:to_list(MinMax)]).


distance_test() ->
    File = csv:binary_reader("../data/iris.txt"),
   #rr_exset {
      features=Features, 
      examples=Examples, 
      exconf=Dataset
     } = rr_example:load(File, 4),
    Dim = list_to_tuple([F || {F, _} <- Features]),
    Flatten = rr_example:flatten(Examples),
    MinMax = min_max(Flatten, Dataset, Dim),
    AllDistances = lists:map(fun (ExId) ->
                                     {ExId, distance(10, ExId, Dim, Dataset, MinMax)}
                             end, Flatten),    
    ?debugFmt("~p: ~p~n", [20, lists:keysort(1, lists:sublist(lists:keysort(2, AllDistances), 10))]).

kd_tree_test() ->
    File = csv:binary_reader("../data/test-kd.txt"),
    #rr_exset {
       features=Features, 
       examples=Examples, 
       exconf=Dataset
      } = rr_example:load(File, 1),
    {Dim, _} = Tree = fit(Examples, Features, Dataset),
    Flatten = rr_example:flatten(Examples),
    Ranges = min_max(Flatten, Dataset, Dim),
    N = knearest(Tree, 1, Dataset, Ranges, 2),
    ?debugFmt("~p~n", [N]),
    throw(error),
    ?assertEqual(#kd_node{
                    axis = 0,
                    left = #kd_node{
                              axis = 1,
                              left = #kd_node{axis = 0, left=leaf, right=leaf, example=1},
                              right =#kd_node{axis = 0, left=leaf, right=leaf, example=4},
                              example = 2},
                    right = #kd_node{
                               axis = 1,
                               left = #kd_node{axis = 0, left = leaf, right = leaf, example = 5},
                               right = leaf,
                               example = 3},
                    example = 6}, element(2, Tree)).

-endif.
