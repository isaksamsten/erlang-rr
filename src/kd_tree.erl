-module(kd_tree).
-compile(export_all).

-include("rr.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(kd_node, {left, right, example}).

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
    Flatten = rr_example:flatten(Examples),
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
    knearest(Example, KdTree, Dim, K, 0, ExSet, Ranges, 0.0).

knearest(Target, KdTree, Dim, K, Depth, ExSet, Ranges, DistanceToParent) ->
    #kd_node{left = Left, right = Right, example = ExampleB} = KdTree,
    DimSize = size(Dim),
    Axis = Depth rem DimSize,
    AxisType = element(Axis + 1, Dim),
    {Nearer, Further} = case less_than(AxisType, rr_example:feature(ExSet, Target, Axis + 1), 
                                     rr_example:feature(ExSet, ExampleB, Axis + 1)) of
                            true ->
                                {Left, Right};
                            false ->
                                {Right, Left}
                        end,
    if Nearer == leaf -> %% We are at the bottom
            Distance = distance(Target, ExampleB, Dim, ExSet, Ranges),
            [{ExampleB, Distance}]; %% return this distance
       true ->
            Distance = distance(Target, ExampleB, Dim, ExSet, Ranges),
            Closest = knearest(Target, Nearer, Dim, K, Depth + 1, ExSet, Ranges, DistanceToParent),
            NewNearest = insert_distance(ExampleB, Distance, Closest, K), %% at upper level
            if Further == leaf ->  %% go cannot but up
                    NewNearest;
               true ->
                    A = rr_example:feature(ExSet, Target, Axis + 1),
                    B = rr_example:feature(ExSet, ExampleB, Axis + 1),
                    DistanceToSplit = DistanceToParent + sqd_point(AxisType, Axis + 1, A, B, Ranges),
                    case length(NewNearest) of
                        Size when Size < K -> % not enought neighbours
                            NewNearest1 = knearest(Target, Further, Dim, K, Depth + 1, ExSet, Ranges, DistanceToSplit),
                            merge_distances(NewNearest, NewNearest1, K);
                        _ ->
                            [Best|_]= NewNearest,
                            if Best >= DistanceToSplit ->
                                    NewNearest1 = knearest(Target, Further, Dim, K, Depth + 1, ExSet, 
                                                           Ranges, DistanceToSplit),
                                    merge_distances(NewNearest, NewNearest1, K);
                               true ->
                                    NewNearest
                            end                    
                    end
            end
    end.

merge_distances(First, Second, K) ->
    S = lists:keysort(2, lists:ukeysort(1, First ++ Second)),
    lists:sublist(S, K).

sqd_point(categoric, _Axis, A, B, _) ->
    math:pow(normalized_diff(categoric, 0, A, B, []), 2);
sqd_point(numeric, Axis, A, B, Ranges) ->
    math:pow(normalized_diff(numeric, Axis, A, B, Ranges), 2).

%% @doc returns {CurrentBest, ListOfBest}
insert_distance(ExId, Distance, Rest, K) when length(Rest) < K ->
    List = [{ExId, Distance}|Rest],
    lists:keysort(2, List);    
insert_distance(ExId, Distance, Rest, K) when length(Rest) == K ->
    [Best|_] = Sorted = lists:keysort(2, [{ExId, Distance}|Rest]),
    [_Worst|Return] = lists:reverse(Sorted),
    [Best|lists:reverse(Return)].
    
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

kd_tree([], _ExSet, _Dim, _Depth) ->
    leaf;
kd_tree(Examples, ExSet, Dim, Depth) ->
    Axis = Depth rem size(Dim),
    Sorted = sort_axis(Axis, element(Axis + 1, Dim), ExSet, Examples),
    {Left, Median, Right} = split_at_median(Sorted, length(Examples) div 2),
    #kd_node {
       left = kd_tree(Left, ExSet, Dim, Depth + 1),
       right = kd_tree(Right, ExSet, Dim, Depth + 1),
       example = Median
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

split_at_median(Examples, Median) ->
    split_at_median(Examples, 0, Median, []).

split_at_median([Median|Right], Current, Middle, Left) when Current == Middle ->
    {Left, Median, Right};
split_at_median([Example|Rest], Current, Middle, Left) ->
    split_at_median(Rest, Current + 1, Middle, [Example|Left]).

testknn() ->
    File = csv:binary_reader("../data/iris.txt"),
   #rr_exset {
      features=Features, 
      examples=Examples, 
      exconf=Dataset
     } = rr_example:load(File, 4),
    {Dim, _} = Tree= fit(Examples, Features, Dataset),
    Flatten = rr_example:flatten(Examples),
    Ranges = min_max(Flatten, Dataset, Dim),
    Rest = knearest(Tree, 10, Dataset, Ranges, 10),
    
    io:format("~p ~n", [lists:keysort(1, Rest)]).

-ifdef(TEST).
real_kd_tree_test() ->
   File = csv:binary_reader("../data/dermatology.txt"),
   #rr_exset {
      features=Features, 
      examples=Examples, 
      exconf=Dataset
     } = rr_example:load(File, 1),
    {Dim, _} = Tree= fit(Examples, Features, Dataset),
    Flatten = rr_example:flatten(Examples),
    Ranges = min_max(Flatten, Dataset, Dim),
    Closest = knearest(Tree, 10, Dataset, Ranges, 10),
    ?debugFmt("~p: ~p ~n", [length(Closest), lists:keysort(1, Closest)]).

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


%% @todo more test cases
is_close_test() ->
    A = insert_distance(1, 0.2, [], 2),
    ?assertEqual({{1, 0.2}, [{1, 0.2}]}, A),
    
    B = insert_distance(2, 0.1, A, 10),
    ?assertEqual({{2,0.1}, [{1,0.2}, {2, 0.1}]}, B),
    
    C = insert_distance(3, 0.01, B, 2),
    ?assertEqual({{3,0.01}, [{2, 0.1}, {3,0.01}]}, C).

distance_test() ->
    File = csv:binary_reader("../data/dermatology.txt"),
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
    Tree = fit(Examples, Features, Dataset),
    ?assertEqual(#kd_node{
                    left = #kd_node{
                              left = #kd_node{left=leaf, right=leaf, example=1},
                              right =#kd_node{left=leaf, right=leaf, example=4},
                              example = 2},
                    right = #kd_node{
                               left = #kd_node{left = leaf, right = leaf, example = 5},
                               right = leaf,
                               example = 3},
                    example = 6}, element(2, Tree)).

-endif.
