%%% @author Isak Karlsson <isak@dhcp-158-243.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created : 23 Oct 2013 by Isak Karlsson <isak@dhcp-158-243.dsv.su.se>

-module(dense_database).
-record(dense_database, {
          features, examples, values, predictions
         }).

-export([
         new/0,
         kill/1,

         value/3,
         vector/2,

         insert_example/2,
         insert_feature/2
        % insert_predition/2
        

         %% todo: for predictions etc
        ]).

new() ->
    #dense_database {
       examples = ets:new(examples, [public, {read_concurrency, true}]),
       features = ets:new(features, [public]),
       predictions = ets:new(predictions, [public]),
       values = ets:new(values, [public])
      }.

kill(Database) ->
    ets:delete(Database#dense_database.features),
    ets:delete(Database#dense_database.examples),
    ets:delete(Database#dense_database.predictions),
    ets:delete(Database#dense_database.values),
    ok.


insert_example(#dense_database{examples = Examples}, Attributes) ->
    ets:insert(Examples, list_to_tuple(Attributes)).

insert_feature(#dense_database{features = Features}, Feature) ->
    ets:insert(Features, Feature).
    
%% @doc get a feature value
value(#dense_database{examples = Examples}, Example, Feature) ->
    ets:lookup_element(Examples, example:id(Example), feature:id(Feature) + 1).

%% @doc get the feature vector for an example
vector(#dense_database{examples = Examples}, Example) ->
    case ets:lookup(Examples, example:id(Example)) of
        [Tuple] -> tl(tuple_to_list(Tuple));
        [] -> false
    end.


