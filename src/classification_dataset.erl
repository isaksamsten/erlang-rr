%%% @author Isak Karlsson <isak@dhcp-158-243.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created : 23 Oct 2013 by Isak Karlsson <isak@dhcp-158-243.dsv.su.se>

-module(classification_dataset).
-include("dataset.hrl").
-behaviour(dataset).
-export([
         load/2,
         load/3,

         value/3,
         vector/2,

         no_features/1,
         no_examples/1,
         
         examples/1,
         features/1,

         split/2,
         merge/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc ....
load(Source, Reader, Options) ->
    NewOptions = [{map, {fun update_class_distribution/2, dict:new()}},
                  {reduce, {fun reduce_class_distributions/2, dict:new()}},
                  {target, class}] ++ Options,
    {Features, Examples, Database} = dataset:load(Source, Reader, NewOptions),
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
load(Source, Reader) ->
    load(Source, Reader, [{cores, 4}]).

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

%% @doc get a feature value
value(#dataset{target = class, database = Database}, Example, Feature) ->
    ets:lookup_element(Database#database.examples, 
                       example:id(Example), 
                       feature:id(Feature) + 1).

%% @doc get the feature vector for an example
vector(#dataset{target = class, database = Database}, Example) ->
    case ets:lookup(Database#database.examples, example:id(Example)) of
        [Tuple] -> tl(tuple_to_list(Tuple));
        [] -> false
    end.

%% @doc get the examples in the dataset
examples(#dataset{target = class, examples = Examples}) ->
    Examples.

%% @doc get the features in the dataset
features(#dataset{target = class, features = Features}) ->
    Features.

%% @doc get number of features
no_features(#dataset{target = class, no_features = NoFeatures}) ->
    NoFeatures.

%% @doc get the number of examples
no_examples(#dataset{target = class, no_examples = NoExamples}) ->
    NoExamples.

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
split(Dataset, {folds, Folds}) ->
    new_folds(examples(Dataset), Dataset, Folds, 1, dict:new());
split(_Dataset, {ratio, _Ratio}) ->
    ok;
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

%% @doc merges a list of [#dataset{},...] into one #dataset
merge(Datasets) ->
    Examples = lists:foldl(fun (Fold, Acc) -> 
                                   lists:zipwith(fun merge/2, examples(Fold), Acc) 
                           end, [], Datasets),
    update_examples(hd(Datasets), Examples).

merge({Class, Ca, Ia}, {Class, Cb, Ib}) ->
    {Class, Ca + Cb, Ia ++ Ib}.

                                   


-ifdef(test).

classification_test_() ->
    dataset_tests().

test_get_featurevalue(Dataset) ->
    ?_assertEqual(5.1, value(Dataset, 1, {numeric, 1})).
    
test_loaded(Dataset) ->
    Features = Dataset#dataset.features,
    Classes = [Class || {Class, _, _} <- Dataset#dataset.examples],
    ?_assertEqual([{numeric,1},{numeric,2},{numeric,3},{numeric,4}], Features),
    ?_assertEqual(['Iris-setosa', 'Iris-versicolor', 'Iris-virginica'], 
                  lists:sort(Classes)).

-endif.
