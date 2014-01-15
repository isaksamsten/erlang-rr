-module(knn).

-export([
         fit/4,
         knearest/3,
         spawn_pknearest/7,
         pknearest/3,
         test/0
        ]).

-include("rr.hrl").

min_max(Features, Examples, ExSet) ->
    Table = ets:new(min_max, [public, {read_concurrency, true}]),
    min_max(Features, Examples, ExSet, Table).

min_max([], _, _, Acc) ->
    Acc;
min_max([{Type, Axis}|Rest], Examples, ExSet, Table) ->
    case Type of
        categoric ->
            min_max(Rest, Examples, ExSet, Table); % skip
        numeric ->
            MinMax = min_max_numeric(Examples, ExSet, Axis, {inf, inf}),
            ets:insert(Table, {Axis, MinMax}),
            min_max(Rest, Examples, ExSet, Table)
    end.

min_max_numeric([], _Database, _Id, MinMax) ->
    MinMax;
min_max_numeric([ExId|Rest], ExSet, Id, {Min, Max}) ->
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
    min_max_numeric(Rest, ExSet, Id, {NewMin, NewMax}).

n_length_chunks_fast(List,Len) ->
    n_length_chunks_fast(lists:reverse(List),[],0,Len).

n_length_chunks_fast([],Acc,_,_) -> Acc;
n_length_chunks_fast([H|T],Acc,Pos,Max) when Pos==Max ->
    n_length_chunks_fast(T,[[H] | Acc],1,Max);
n_length_chunks_fast([H|T],[HAcc | TAcc],Pos,Max) ->
    n_length_chunks_fast(T,[[H | HAcc] | TAcc],Pos+1,Max);
n_length_chunks_fast([H|T],[],Pos,Max) ->
    n_length_chunks_fast(T,[[H]],Pos+1,Max).

fit(Features, Examples, ExSet, Cores) ->
    Flatten = rr_example:flatten(Examples),
    Parts = n_length_chunks_fast(Flatten, length(Flatten) div Cores),
    Ranges = min_max(Features, Flatten, ExSet),
    {linear, Features, Parts, ExSet, Ranges, length(Parts)}.
    
knearest({linear, Features, Flatten, ExSet, Ranges}, Example, K) ->
    AllDistances = lists:foldl(fun (ExId, Acc) ->
                                       ExId2 =  rr_example:exid(Example),
                                       if ExId == ExId2 ->
                                               Acc;
                                          true ->
                                               [{ExId, distance(Features, Example, ExId, ExSet, Ranges, 0.0)}|Acc]
                                       end
                               end, [], Flatten),    
    lists:sublist(lists:keysort(2, AllDistances), K).


pknearest({linear, Features, Flatten, ExSet, Ranges, Cores}, Example, K) ->
    Self = self(),
    lists:foreach(fun (Part) ->
                          spawn_link(fun () -> 
                                             spawn_pknearest(Self, Features, Part, ExSet, Ranges, Example, K) 
                                     end)
                  end, Flatten),
    collect_knearest(Cores, K, []).

collect_knearest(0, K, Acc) ->
    lists:sublist(lists:keysort(2, Acc), K);
collect_knearest(Cores, K, Acc) ->
    Self = self(),
    receive
        {done, Self, Near} ->
            collect_knearest(Cores - 1, K, Acc ++ Near)
    end.


spawn_pknearest(Self, Features, Part, ExSet, Ranges, Example, K) ->
    Nearest = knearest({linear, Features, Part, ExSet, Ranges}, Example, K),
    Self ! {done, Self, Nearest}.

    

distance([], _, _, _, _, Acc) ->
    Acc;
distance([{Type, Axis}|Rest], Ex1, Ex2, ExSet, Ranges, Acc) ->
    A = rr_example:feature(ExSet, Ex1, Axis),
    B = rr_example:feature(ExSet, Ex2, Axis),
    NewAcc = Acc + math:pow(normalized_diff(Type, Axis, A, B, Ranges), 2),
    distance(Rest, Ex1, Ex2, ExSet, Ranges, NewAcc).
    
normalized_diff(numeric, Current, A, B, Ranges) ->
    Range = ets:lookup_element(Ranges, Current, 2),
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

test() ->
    File = csv:binary_reader("../data/yeast.txt"),
    #rr_exset {
       features=Features, 
       examples=Examples, 
       exconf=Dataset
      } = rr_example:load(File, 4),
    Model = fit(Features, Examples, Dataset, 4),
    {Time, Rest} = timer:tc(fun () -> pknearest(Model, 10, 15) end),
    
    io:format("~p ~p ~n", [Time, lists:keysort(2, Rest)]).
