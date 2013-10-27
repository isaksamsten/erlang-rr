%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% Module for handling the examples
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_example).

-compile(export_all).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-endif.

%% @headerfile "rr.hrl"
-include("rr.hrl").

%% @doc insert a prediction into the global table of all predictions
-spec insert_prediction(#rr_example{}, exid(), any()) -> ok.
insert_prediction(Conf, ExId, Pred) ->
    ets:insert(Conf#rr_example.predictions, {exid(ExId), Pred}).

get_prediction(Conf, ExId) ->
    hd(ets:lookup(Conf#rr_example.predictions, exid(ExId))).

%% @doc get predictions for all examples in Examples
-spec predictions(#rr_example{}, examples()) -> [{exid(), [{Class::atom(), Probability::number()},...]},...].
predictions(Conf, Examples) ->
    predictions(Conf#rr_example.predictions, Examples, []).

predictions(_, [], Acc) -> Acc;
predictions(Table, [{Real, _, Ids}|Rest], Acc) ->
    predictions(Table, Rest,
                lists:foldl(
                  fun (ExId, NewAcc) ->
                          Predictions = ets:lookup_element(Table, exid(ExId), 2),
                          [{exid(ExId), Real, Predictions}|NewAcc]
                  end, Acc, Ids)).

%% @doc create new example dataset
new() ->
    #rr_example{
       examples = ets:new(examples, [public, {read_concurrency, true}]),
       features = ets:new(features, [public]),
       predictions = ets:new(predictions, [public]),
       values = ets:new(values, [public])
      }.

%% @doc delete a dataset
kill(#rr_exset{exconf=Dataset}) ->
    kill(Dataset);
kill(Dataset) ->
    #rr_example {
       features = FeatureTable, 
       examples = ExTable, 
       values = ExValues,
       predictions = PredictionsTable } = Dataset,

    ets:delete(FeatureTable),
    ets:delete(ExTable),
    ets:delete(ExValues),
    ets:delete(PredictionsTable).

%% @doc a dataset from File using Core cores (creates a new dataset)
-spec load(string(), number()) -> {features(), examples(), #rr_example{}}.
load(File, Core) ->
    ExConf = new(),
    {Features, Examples} = load(File, Core, ExConf),
    #rr_exset {
       features = Features,
       examples = Examples,
       exconf = ExConf
      }.

%% @doc load a dataset from file using Core cores to Dataset 
-spec load(string(), number(), #rr_example{}) -> {features(), examples()}.
load(File, Cores, Dataset) ->
    #rr_example{features = FeatureTable} = Dataset,
    {ClassId, Types} = case csv:next_line(File) of
                           {row, Types0, _} ->
                               parse_type_declaration(Types0);
                           eof ->
                               throw({error, features_type_error})
                       end,
    Features = case csv:next_line(File) of
                   {row, Features0, _} ->
                       parse_feature_declaration(FeatureTable, Features0, ClassId, Types);
                   eof ->
                       throw({error, features_type_error})
               end,
    Examples = parse_examples(Dataset, File, Cores, ClassId, Types),
    {Features, Examples}.
    
%% @private spawns "Cores" 'parse_example_process' and collects their results
parse_examples(Dataset, File, Cores, ClassId, Types) ->
    Self = self(),
    ExTable = Dataset#rr_example.examples,
    Handler = spawn_link(?MODULE, example_value_handler, [Dataset, dict:new()]),
    lists:foreach(fun (_) ->
                          spawn_link(?MODULE, parse_example_process, 
                                     [Self, ExTable, File, ClassId, Types, Handler, dict:new()])
                  end, lists:seq(1, Cores)),
    Examples = collect_parse_example_processes(Self, Cores, dict:new()),
%    Handler ! {stop, Self, rr_example:count(Examples)},
 %   receive
  %     {value_handler_stop, Self} ->
    Examples.
 %   end.



%% @private process that gets a line from the "File" and process each example
parse_example_process(Parent, ExTable, File, ClassId, Types, Handler, Acc) ->
    case csv:next_line(File) of
        {row, Example, Id0} ->
            {Class, Attributes} = take_feature(Example, ClassId),
            Id = Id0 - 2,
            insert_features(ExTable, Attributes, Types, Handler, Id),
            parse_example_process(Parent, ExTable, File, ClassId, Types, Handler,
                                  update_class_distribution(Class, Id, Acc));
        eof ->
            Parent ! {done, Parent, Acc}
    end.

insert_features(ExTable, Attributes, Types, Handler, Id) ->
    ets:insert(ExTable, format_features(Attributes, Types, 1, Handler, [Id])).
    
%% @private collect the results from process parsing the examples
collect_parse_example_processes(_, 0, Examples) ->
    format_class_distribution(Examples);
collect_parse_example_processes(Self, Cores, Examples) ->
    receive
        {done, Self, Part} ->
            collect_parse_example_processes(Self, Cores - 1, 
                                            dict:merge(fun (_, {CountA, IdsA}, {CountB, IdsB}) ->
                                                               {CountA + CountB, IdsA ++ IdsB}
                                                       end, Examples, Part))
    end.

%% @private format example values according to their correct type
format_features([], [], _, _, Acc) ->
    list_to_tuple(lists:reverse(Acc));
format_features([Value|Values], [categoric|Types], Column, Handler, Acc) ->
    FeatureValue = if Value == "?" -> '?'; true -> list_to_binary(Value) end,
%    Handler ! {categoric, Column, FeatureValue},
    format_features(Values, Types, Column + 1, Handler, [FeatureValue|Acc]);
format_features([Value|Values], [numeric|Types], Column, Handler, Acc) ->
    format_features(Values, Types, Column + 1, Handler,
                    [case format_number(Value) of
                         {true, Number} ->
%                            Handler ! {numeric, Column, Number},
                             Number;
                         '?' ->
                             '?';
                         false ->
                             throw({error, {invalid_number_format, Column, Value}})
                     end|Acc]).

example_value_handler(ExTable, Dict) ->
    receive
        {numeric, Feature, Value} ->
            NewDict = dict:update_counter(Feature, Value, Dict),
            example_value_handler(ExTable, NewDict);
        {categoric, Feature, Value} ->
            NewDict = dict:update(Feature, fun (ValuesDict) -> 
                                                   dict:update_counter(Value, 1, ValuesDict) 
                                           end, dict:store(Value, 1, dict:new()), Dict),
            example_value_handler(ExTable, NewDict);
        {stop, Parent, NoEx} ->
            Values = ExTable#rr_example.values,
            dict:fold(fun (Key, Value, _) when is_number(Value) ->
                              ets:insert(Values, {Key, numeric, Value/NoEx});
                          (Key, Value, _) ->
                              NewValue = rr_util:max(fun ({K, V}) -> if K == '?' -> 0; true -> V end end, dict:to_list(Value)),
                              ets:insert(Values, {Key, categoric, NewValue})
                      end, 0, Dict),
            Parent ! {value_handler_stop, Parent}
    end.

%% @doc determine if a string is a number or missing (?)
format_number("?") ->
    '?';
format_number(N) when is_number(N) ->
    {true, N};
format_number(L0) ->
    L = if is_binary(L0) -> binary_to_list(L0); true -> L0 end,
    Float = (catch erlang:list_to_float(L)),
    case is_number(Float) of
        true ->
            {true, Float};
        false ->
            Int = (catch erlang:list_to_integer(L)),
            case is_number(Int) of
                true ->
                    {true, Int};
                false ->
                    false
            end
    end.

%% @private format a class distribution by sorting the list formed by a dict()
format_class_distribution(Examples) ->
    lists:keysort(1, lists:map(fun ({Class, {Count, Ids}}) ->
                                       {Class, Count, Ids}
                               end, dict:to_list(Examples))).

%% @private merge two dict() with class distributions
update_class_distribution(Class, Id, Acc) ->
    dict:update(Class, fun({Count, Ids}) ->
                               {Count + 1, [Id|Ids]}
                       end, {1, [Id]}, Acc).
    
%% @private parse type declaration and return {ClassId, [types(),...]}
parse_type_declaration(Types) ->
    parse_type_declaration(Types, missing, missing, 1, []).

parse_type_declaration([], ClassId, _IdId, _, Acc) ->
    {ClassId, lists:reverse(Acc)};
parse_type_declaration([Type0|Rest], ClassId, IdId, Id, Acc) ->
    Type = list_to_atom(string:to_lower(Type0)),
    case Type of
        Type when Type =:= numeric;
                  Type =:= categoric ->
            parse_type_declaration(Rest, ClassId, IdId, Id + 1, [Type|Acc]);
        Type when Type =:= class;
                  ClassId =:= missing ->
            parse_type_declaration(Rest, Id, IdId, Id + 1, Acc);
        Type when Type =:= id ->
            parse_type_declaration(Rest, ClassId, Id, Id + 1, Acc); % NOTE: not working
        _ ->
            throw({error, {invalid_type_declaration, Id}})
    end.

%% @private parse a feature declaration
parse_feature_declaration(FeatureTable, Features0, ClassId, Types) ->
    {_, Features} = take_feature(Features0, ClassId),
    if length(Features) =/= length(Types) ->
            throw({error, {invalid_feature_declaration, {length(Features), '/=', length(Types)}}});
       true ->
            parse_feature_declaration(FeatureTable, Features, Types, 1, [])
    end.
parse_feature_declaration(_, [], [], _, Acc) ->
    lists:reverse(Acc);
parse_feature_declaration(FeatureTable, [Feature|Features], [Type|Types], Id, Acc) ->
    ets:insert(FeatureTable, {Id, Feature}),
    parse_feature_declaration(FeatureTable, Features, Types, Id + 1, [{Type, Id}|Acc]).

take_feature(A, missing) ->
    {'?', A};
take_feature([A|R], 1) ->
    {list_to_atom(A), R};
take_feature(List, N) ->
    {L1, [Item|L2]} = lists:split(N - 1, List),
    {list_to_atom(Item), L1 ++ L2}.

count(ExId) when is_number(ExId) ->
    1;
count({ExId, N}) when is_number(ExId) ->
    N;
count(Examples) ->
    lists:foldl(fun({_, Count, _}, Old) ->
                        Count + Old
                end, 0, Examples).

%% @doc clone example id
clone(ExId) ->
    {exid(ExId), exid(ExId)}.

%% @doc return the id of an example (unpacks an example with its count)
exid({ExId, _}) ->
    ExId;
exid(ExId) ->
    ExId.

%% @doc count the occurences of "Class" in "Examples"
count(Class, Examples) ->
    case lists:keysearch(Class, 1, Examples) of
        {value, {_, N, _}} ->
            N;
        _ -> 
            0
    end.

%% @doc return the majority class and its count
-spec majority(examples()) -> {Class::atom(), Count::number()}.
majority(Examples) ->
    {Class, Count, _} = rr_util:max(fun({_, Count, _}) -> Count end, Examples),
    {Class, Count}.
                            
    %% {Class, Count, _} = lists:foldl(fun({Class, Count, _}, {_OldClass, OldCount, _} = Old) ->
    %%                                      case Count > OldCount of 
    %%                                          true -> {Class, Count, []};
    %%                                          false -> Old
    %%                                      end
    %%                              end, hd(Examples), tl(Examples)),
    %% {Class, Count}.

%% @doc get examples with Class
get_class(Class, Examples) ->
    lists:keyfind(Class, 1, Examples).

%% @doc get the number of classes
classes(Examples) ->
    length(Examples).

%% @doc flatten the examples (i.e. make one list)
flatten(Examples) ->
    lists:foldl(fun ({_, _, Ex}, Acc) ->
                        Ex ++ Acc
                end, [], Examples).

%% @doc Count the number of examples in "Examples" excluding examples with Class
count_exclude(Class, Examples) ->
    lists:foldl(fun({Cls, _, _}, Old) when Class =:= Cls->
                        Old;
                   ({_, Count, _}, Old) ->
                        Old + Count
                end, 0, Examples).

%% @doc transform the examples into a form where we have a set of positive and a set of negative examples
to_binary(Positive, Examples) ->
    case lists:keytake(Positive, 1, Examples) of
        {value, {_, Pc, Positives}, Negatives0} ->
            [{'+', Pc, Positives}, lists:foldl(fun({_, Nc, Ids}, {_, N, Acc}) ->
                                                       {'-', Nc+N, Acc ++ Ids}
                                               end, {'-', 0, []}, Negatives0)];
        false ->
            throw({error, cannot_split})
    end.

%% @doc Remove Examples from "Examples" that are covered by "Covered"
remove_covered(Examples, Covered) ->
    lists:map(fun({Class, Count, Ids}) ->
                      case rr_example:get_class(Class, Covered) of
                          {Class, Count0, Ids0} ->
                              NewIds = gb_sets:to_list(gb_sets:subtract(gb_sets:from_list(Ids),
                                                                        gb_sets:from_list(Ids0))),
                              {Class, Count - Count0, NewIds};
                          _ ->
                              {Class, Count, Ids}
                      end
              end, Examples).

%% @doc Return a tuple {Pos, Neg} with the number of Positive and negative examples
-spec coverage(examples()) -> {Pos::number(), Neg::number()}.
coverage(Examples) ->
    {rr_example:count('+', Examples), rr_example:count('-', Examples)}.

%% @doc return the value for ExId at positition At
-spec feature(#rr_example{}, exid(), number()) -> ok.
feature(#rr_example{examples=ExTable}, ExId, At) ->
    ets:lookup_element(ExTable, exid(ExId), At + 1).

%% @doc return the entire feature vector for ExId
-spec vector(#rr_example{}, exid()) -> tuple().
vector(#rr_example{examples=ExTable}, ExId) ->
    hd(ets:lookup(ExTable, exid(ExId))).

%% @doc unpack the feature id
feature_id({{combined, IdA, IdB}, _}) ->
    list_to_tuple(lists:sort([feature_id(IdA), feature_id(IdB)]));
feature_id({{_, Id}, _}) ->
    Id;
feature_id({rule, {Rules, _}, _Length}) ->
    Ids = [feature_id(Rule) || Rule <- Rules],
    lists:sort(Ids);
feature_id({_, Id}) ->
    Id.

%% @doc get the name of a feature id as returned by feature_id/1
feature_name(Conf, {IdA, IdB}) ->
    {feature_name(Conf, IdA),
     feature_name(Conf, IdB)};
feature_name(Conf, Rules) when is_list(Rules) ->
    [feature_name(Conf, Rule) || Rule <- Rules];
feature_name(#rr_example{features=FeatureTable}, Id) ->
    ets:lookup_element(FeatureTable, Id, 2).

%% @doc Return a random subset of size "Subset" from Features
random_features(Features, 1) when length(Features) > 1 ->
    [lists:nth(random:uniform(length(Features)), Features)];
random_features(Features, Subset) when Subset > length(Features) ->
    Features;
random_features(Features, Subset) ->
    {Top, _} = lists:split(Subset, shuffle(Features)),
    Top.

%% @doc Return the dataset splitted into {Train, Test} with "Ratio" denoting the size of Train
split_dataset(Examples, Ratio) ->
    lists:foldl(fun({Class, Count, Ids}, 
                    {TrainAcc, TestAcc}) ->
                        {Train, Test} = lists:split(round(Count * Ratio), Ids),
                        {[{Class, length(Train), Train}|TrainAcc],
                         [{Class, length(Test), Test}|TestAcc]}
                end, {[], []}, Examples).

%% @doc 
%% take a subset of Size from Examples. If size is within [0, 1],
%% is taken as a fraction, o/w Size is absolute
%% @doc
subset(Examples, Size) when is_float(Size), Size =< 1.0, Size >= 0.0 ->
    Subset = lists:foldl(fun ({Class, _Count, Ids}, Acc) ->
                                 Fraction = round(length(Ids) * Size),
                                 {Take, _Leave} = lists:split(Fraction, Ids),
                                 [{Class, Fraction, Take}|Acc]
                         end, [], Examples),
    lists:filter(fun ({_, A, _}) ->
                         A > 0
                 end, Subset);
subset(Examples, Size) ->
    Total = count(Examples),
    Fraction = Size/Total,
    if Fraction >= 1 -> Examples; true -> subset(Examples, Fraction) end.

%% @doc shuffle the data set (for example, before splitting)
-spec randomize(examples()) -> examples().
randomize(Examples) ->
    lists:foldl(fun({Class, Count, Ids}, Acc) ->
                        [{Class, Count, shuffle_list(Ids)}|Acc]
                end, [], Examples).

%% @doc randomly permute a list (public)
shuffle(List) ->
    shuffle_list(List).

%% @doc randomly permute a list of items
shuffle_list(Ids0) ->
    [Id || {_, Id} <- lists:keysort(1, lists:map(fun (Id) -> {random:uniform(), Id} end, Ids0))].



-ifdef(TEST).

%% mock_examples(Mock) ->
%%     lists:foldl(fun ({Class, Count}, Acc) ->
%%                         [{Class, Count, lists:seq(1, Count)}|Acc]
%%                 end, [], Mock).

%% mock_split(Left, Right) ->
%%     {both, mock_examples(Left), mock_examples(Right)}.

%% split_test() ->
%%     random:seed({1,2,3}),
%%     Examples = mock_examples([{a, 10}, {b, 10}]),
%%     D = fun (_, _, _) ->
%%                 R = random:uniform(),
%%                 if R =< 0.5 -> {left, 1}; true -> {right, 1} end
%%         end,
%%     M = fun (_, _, _, _, _) ->
%%                 ignore
%%         end,
%%     S = fun (_, _, _) ->
%%                 0.5
%%         end,
%%     Split = split(#rr_example{}, 1, Examples, D, M, S),
%%     ?assertEqual(Split, {0.5, {both,[{b,5,[8,7,6,3,1]},{a,4,[10,5,4,3]}],
%%                                [{b,5,[10,9,5,4,2]},{a,6,[9,8,7,6,2,1]}]}}),

%%     OnlyOne = mock_examples([{a, 10}]),
%%     Split0 = split(#rr_example{}, 1, OnlyOne, D, M, S),
%%     ?assertEqual(Split0, {0.5,{both,[{a,4,[9,7,4,2]}],[{a,6,[10,8,6,5,3,1]}]}}).

%% best_split_test() ->
%%     random:seed({1,2,3}),
%%     Examples = mock_examples([{a, 10}, {b, 10}]),
%%     D = fun (_, _, _) ->
%%                 R = random:uniform(),
%%                 if R =< 0.5 -> {left, 1}; true -> {right, 1} end
%%         end,
%%     M = fun (_, _, _, _, _) ->
%%                 ignore
%%         end,
%%     S = fun (_, _, _) ->
%%                 0.5
%%         end,
%%     Split = fun (M, F, E, Dm, DM) ->
%%                     split(M, F, E, Dm, DM, S)
%%             end,
%%     Cand = best_split(#rr_example{}, [1,2], Examples, 20, rf_tree:info(), Split, D, M),
%%     ?assertEqual(Cand#candidate.feature, {2, 0.5}),
%%     ?assertEqual(Cand#candidate.score, {13.46023334018513,6.730116670092565,6.730116670092565}).

%% subset_test() ->
%%     Examples = mock_examples([{a, 10}, {b, 20}]),
%%     Subset = subset(Examples, 0.2),
%%     ?assertEqual(count(Subset), round(count(Examples)*0.2)),
    
%%     Subset0 = subset(Examples, 10),
%%     ?assertEqual(count(Subset0), 10),
%%     ?assertEqual(count(subset(Examples, 30)), 30),
%%     ?assertEqual(count(subset(Examples, 33)), 30),
%%     ?assertEqual(subset(mock_examples([{a, 10}, {b, 2}]), 0.2), [{a, 2, [1,2]}]).

%% bootstrap_test() ->
%%     random:seed({1,2,3}),
%%     Examples = mock_examples([{a, 1000}, {b, 2000}]),
%%     {InBag, OutBag} = rr_sampling:bootstrap_replicate(Examples),
%%     ?assertEqual(count(a, InBag), 1017),
%%     ?assertEqual(count(b, InBag), 1983),
    
%%     SumInBagA = lists:sum([count(C) || C <- element(3, hd(InBag))]),
%%     SumInBagB = lists:sum([count(C) || C <- element(3, hd(tl(InBag)))]),
%%     ?assertEqual(count(a, InBag), SumInBagA),
%%     ?assertEqual(count(b, InBag), SumInBagB),

%%     SumOutBagA = lists:sum([count(C) || C <- element(3, hd(OutBag))]),
%%     SumOutBagB = lists:sum([count(C) || C <- element(3, hd(tl(OutBag)))]),
%%     ?assertEqual(count(a, OutBag), SumOutBagA),
%%     ?assertEqual(count(b, OutBag), SumOutBagB),
%%     ?assertEqual(count(Examples), SumInBagA + SumInBagB),
%%     ?assertEqual(round((SumOutBagA + SumOutBagB) / (SumInBagA + SumInBagB) * 100)/100 , 0.36).
    
%% majority_test() ->
%%     Examples = mock_examples([{a, 100}, {b, 10}, {c, 1000}]),
%%     ?assertEqual({c, 1000}, majority(Examples)).

-endif.
