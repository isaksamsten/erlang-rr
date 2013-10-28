%%% @author Isak Karlsson <isak@dhcp-158-243.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created : 23 Oct 2013 by Isak Karlsson <isak@dhcp-158-243.dsv.su.se>

-module(classification_dataset).
-include("rr.hrl").

-behaviour(dataset).
-export([
         load/1,
         load/2,

         update_examples/2,

         split/2,
         merge/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc ....
load(Source, Options) ->
    NewOptions = [{map, {fun update_class_distribution/2, dict:new()}},
                  {reduce, {fun reduce_class_distributions/2, dict:new()}},
                  {target, class}] ++ Options,
    {Features, Examples, Database} = dataset:load(Source, NewOptions),
    FormattedExamples = format_class_distribution(Examples),
    #dataset {
       target = class,
       module = ?MODULE,
       features = Features,
       no_features = length(Features),
       no_examples = count(FormattedExamples),
       examples = FormattedExamples,
       database = Database
      }.

%% @doc ....
load(Source) ->
    load(Source, [{cores, 4}]).

%% @private format a class distribution by sorting the list formed by a dict()
format_class_distribution(Examples) ->
    lists:keysort(1, lists:map(fun ({Class, {Count, Ids}}) ->
                                       {Class, Count, Ids}
                               end, dict:to_list(Examples))).

%% @private merge two dict() with class distributions
update_class_distribution({Class, Id}, Acc) ->
    dict:update(Class, fun({Count, Ids}) ->
                               {Count + 1, [Id|Ids]}
                       end, {1, [Id]}, Acc).

%% @private reduce the class distributions to a list of class-tuples
reduce_class_distributions(Part, Acc) ->
    dict:merge(fun (_, {CountA, IdsA}, {CountB, IdsB}) ->
                       {CountA + CountB, IdsA ++ IdsB}
               end, Acc, Part).

%% @private
count(Examples) ->
    lists:foldl(fun ({_, C, _}, Acc) -> C + Acc end, 0, Examples).

%% @private update the example ids in Dataset
update_examples(Dataset, Examples) ->
    Dataset#dataset{examples = Examples, no_examples = count(Examples)}.

%% @doc
%%
%% Split a dataset into [2..n] parts. If called with {ratio,
%% float()}. the result is a tuple with two parts {training(),
%% testing()}. If called with {folds, integer()}, the result is a
%% dict() with keys corresponding the a particular fold and the value
%% to a specific part of the dataset.
%%
%% Both versions are stratified
%% @end
-spec split(#dataset{}, {ratio | folds, number()}) -> any().
split(Dataset, {folds, Folds}) when Folds > 1 ->
    new_folds(dataset:examples(Dataset), Dataset, Folds, 1, dict:new());
split(Dataset, {ratio, Ratio}) when Ratio < 1 andalso Ratio > 0 ->
    Examples = dataset:examples(Dataset),
    {Train, Test} = lists:foldl(fun({Class, Count, Ids}, 
                                    {TrainAcc, TestAcc}) ->
                                        {Train, Test} = lists:split(round(Count * Ratio), Ids),
                                        {[{Class, length(Train), Train}|TrainAcc],
                                         [{Class, length(Test), Test}|TestAcc]}
                                end, {[], []}, Examples),
    {update_examples(Dataset, Train),
     update_examples(Dataset, Test)};
split(Dataset, Folds) when is_integer(Folds) ->
    split(Dataset, {folds, Folds});
split(Dataset, Ratio) when is_float(Ratio) ->
    split(Dataset, {ratio, Ratio}).

%% @private
new_folds([], Dataset, _, _, Acc) ->
    dict:map(fun (_, Examples) -> update_examples(Dataset, Examples) end, Acc);
new_folds([{Class, _, ExIds}|Rest], Dataset, Folds, Fold, Acc) ->
    {ClassFolds, NewFold} = new_folds_for_class(Class, ExIds, Folds, Fold,
                                                new_fold(Folds, Class, dict:new())),
    NewAcc = dict:merge(fun (_, New, Old) -> New ++ Old end, ClassFolds, Acc),
    new_folds(Rest, Dataset, Folds, NewFold, NewAcc).
                     
%% @private
new_fold(0, _, Acc) ->
    Acc;
new_fold(Fold, Class, Acc) ->
    new_fold(Fold - 1, Class, dict:store(Fold, [{Class, 0, []}], Acc)).

new_folds_for_class(_, [], _, CurrentFold, Acc) ->
    {Acc, CurrentFold};
new_folds_for_class(Class, [Id|Examples], Folds, CurrentFold, Acc) ->
    Current = if Folds < CurrentFold -> 1; true -> CurrentFold end,
    new_folds_for_class(Class, Examples, Folds, Current + 1,
                        dict:update(Current, fun ([{_, N, Ids}]) ->
                                                     [{Class, N+1, [Id|Ids]}]
                                             end, Acc)).

%% @doc merges a list of [#dataset{}, #dataset{},...] into one #dataset
merge([Dataset]) ->
    Dataset;
merge(Datasets) ->
    First = hd(Datasets),
    Examples = lists:foldl(
                 fun (Fold, Acc) -> 
                         lists:zipwith(fun merge/2, dataset:examples(Fold), Acc) 
                 end, dataset:examples(First), tl(Datasets)),
    update_examples(First, Examples).

%% @private
merge({Class, Ca, Ia}, {Class, Cb, Ib}) ->
    {Class, Ca + Cb, Ia ++ Ib}.

-ifdef(TEST).

classification_test_() ->
    dataset_tests().

dataset_tests() ->
    Dataset = load(csv:binary_reader("../data/iris.txt")),
    [
     {"Test loading",
      {timeout, 30, test_loaded(Dataset)}},
     {"Test feature value",
      {timeout, 30, test_get_featurevalue(Dataset)}},
     {"Test split into folds",
      {timeout, 30, test_split_folds(Dataset)}},
     {"Test split into train test",
      {timeout, 30, test_split_ratio(Dataset)}}
    ] ++ [{lists:flatten(io_lib:format("Test merge ~p folds", [F])),
           {timeout, 30, test_merge(Dataset, F)}} || F <- lists:seq(2, 10)].

test_split_folds(Dataset) ->
    Folds = dataset:split(Dataset, {folds, 10}),
    ?_assertEqual(10, dict:size(Folds)).

test_split_ratio(Dataset) ->
    {Train, Test} = dataset:split(Dataset, {ratio, 0.5}),
    ?_assert(dataset:no_examples(Train) == dataset:no_examples(Test)).

test_merge(Dataset, F) ->
    Folds = dataset:split(Dataset, {folds, F}),
    First = dict:fetch(1, Folds),
    ToMerge = dict:fold(fun (_Fold, Fold, Acc) -> [Fold|Acc] end, [], dict:erase(1, Folds)),
    Merge = merge(ToMerge),
    ?_assertEqual(150-dataset:no_examples(First), dataset:no_examples(Merge)).

test_get_featurevalue(Dataset) ->
    ?_assertEqual(5.1, dataset:value(Dataset, 1, {numeric, 1})).
    
test_loaded(Dataset) ->
    Features = dataset:features(Dataset),
    Classes = [Class || {Class, _, _} <- dataset:examples(Dataset)],
    ?_assertEqual([{numeric,1},{numeric,2},{numeric,3},{numeric,4}], Features),
    ?_assertEqual(['Iris-setosa', 'Iris-versicolor', 'Iris-virginica'], 
                  lists:sort(Classes)).

-endif.
