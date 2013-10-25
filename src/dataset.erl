%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% Dataset = dataset:load(csv:binary_reader("file.csv"),
%%%                       [{cores, 4}, 
%%%                        {map, {fun ..., Acc}}, 
%%%                        {reduce, {fun ..., Acc}},
%%%                        {target, ...}])
%%% Classification = classification_dataset:load(csv:binary_reader("file.csv")).
%%% Regression = regression_dataset:load(csv:binary_reader("file.csv").
%%%
%%% @end
%%% Created : 23 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(dataset).
-include("rr.hrl").

-export([
         behaviour_info/1
        ]).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @private
behaviour_info(callbacks) ->
    [
     {load, 1}, 
     {load, 2},

     {value, 3},
     {vector, 2},

     {examples, 1},
     {features, 1},

     {no_examples, 1},
     {no_features, 1},
     
     {merge, 1},
     {split, 2}
    ].

%% @doc create a new dataset
new_database() ->
    #database {
       examples = ets:new(examples, [public, {read_concurrency, true}]),
       features = ets:new(features, [public]),
       predictions = ets:new(predictions, [public]),
       values = ets:new(values, [public])
      }.

%% @doc 
%% reclaim the memory occupied by the ets tables. This should always
%% be done for used dataset.
%% @end
kill(#dataset{database = Database}) ->
    ets:delete(Database#database.features),
    ets:delete(Database#database.examples),
    ets:delete(Database#database.predictions),
    ets:delete(Database#database.values).

%% @doc
%% Load Source file
%% 
%% === Options ===
%% ```[ 
%%     {cores, integer()}, 
%%     {map, fun({integer(), integer()}, Acc::any()) -> Acc::any(), Acc::any()},
%%     {reduce, fun(MapPart::any(), Acc::any()) -> Acc::any(), Acc:any()},
%%     {target, class | regression | atom()},
%%     {store_example, fun(integer(), Attributes::list()) -> ok},
%%     {store_features, fun(integer(), Feature::tuple()) -> ok} % not implemented
%%    ]'''
%% where (sensible) default options are provided for `store_examples' and `cores'.
%%
%% @end
load({Reader, Source}, Options) ->
    Database = new_database(),
    Cores = proplists:get_value(cores, Options, erlang:system_info(schedulers)),
    Map = proplists:get_value(map, Options),
    Reduce = proplists:get_value(reduce, Options),
    Target = proplists:get_value(target, Options),
    Store = proplists:get_value(store_example, Options, fun insert_features/2),
    io:format("~p ~n",[Source]),
    load(Reader, Source, Database, Map, Reduce, Store, Target, Cores).

%% @private
load(Reader, Source, Database, Map, Reduce, Store, Target, Cores) ->
    {ok, TargetId, Types} = parse_type_declaration(element(2, Reader:next_line(Source)), Target),
    {ok, Features} = parse_feature_declaration(element(2, Reader:next_line(Source)), Types, TargetId,
                                               Database#database.features),
    {ok, Examples} = parse_examples(Source, Reader, Database, Types, Map, Reduce, Store, TargetId, Cores),
    {Features, Examples, Database}.

%% @private parse a type declaration
parse_type_declaration(eof, _) ->
    {error, bad_types};
parse_type_declaration(Types, Target) ->
    parse_type_declaration(Types, missing, missing, 1, Target, []).

parse_type_declaration([], TargetId, _IdId, _, _, Acc) ->
    {ok, TargetId, lists:reverse(Acc)};
parse_type_declaration([Type0|Rest], ClassId, Id, Inc, Target, Acc) ->
    Type = list_to_atom(string:to_lower(Type0)),
    case Type of
        Type when Type =:= numeric; Type =:= categoric ->
            parse_type_declaration(Rest, ClassId, Id, Inc + 1, Target, [Type|Acc]);
        Type when Type =:= Target; ClassId =:= missing ->
            parse_type_declaration(Rest, Inc, Id, Inc + 1, Target, Acc);
        Type when Type =:= id ->
            parse_type_declaration(Rest, ClassId, Id, Inc, Target, Acc); % NOTE: not working
        _ ->
            {error, {bad_types, Id}}
    end.

%% @private parse a feature declaration according to the type declaration
parse_feature_declaration(Features0, Types, TargetId, FeatureDatabase) ->
    {TargetName, Features} = cherrypick(Features0, TargetId),
    ets:insert(FeatureDatabase, {0, TargetName}),
    if length(Features) =/= length(Types) ->
            {error, {bad_features, length(Features)}};
       true ->
            {ok, parse_feature_declaration(Features, Types, FeatureDatabase, 1, [])}
    end.

%% @private
parse_feature_declaration([], [], _, _, Acc) ->
    lists:reverse(Acc);
parse_feature_declaration([Feature|Features], [Type|Types], FeatureDatabase, Inc, Acc) ->
    ets:insert(FeatureDatabase, {Inc, Feature}),
    parse_feature_declaration(Features, Types, FeatureDatabase, Inc + 1, [{Type, Inc}|Acc]).

%% @private 
parse_examples(Source, Reader, Database, Types, Map, Reduce, Store, TargetId, Cores) ->
    spawn_parse_example_processes(Source, Reader, Database, Types, Map, Store, TargetId, Cores),
    collect_parse_example_processes(self(),Cores, Reduce).

%% @private
spawn_parse_example_processes(Source, Reader, Database, Types, Map, Store, TargetId, Cores) ->
    Self = self(),
    [spawn_link(
       fun() -> parse_example_process(Self, Source, Reader, Database,
                                      Types, TargetId, Store, Map) end)
     || _ <-lists:seq(1, Cores)].

%% @private
parse_example_process(Parent, Source, Reader, Database, Types, TargetId, Store, {Map, Acc}) ->
    case Reader:next_line(Source) of
        {row, Example, Row} ->
            {Class, Attributes} = cherrypick(Example, TargetId),
            Id = Row - 2,
            AttributeList = parse_example_attributes(Attributes, Types, 1, [Id]),
            ok = Store(Database, AttributeList),
            NewAcc = Map({Class, Id}, Acc),
            parse_example_process(Parent, Source, Reader, Database, 
                                  Types, TargetId, Store, {Map, NewAcc});
        eof ->
            Parent ! {done, Parent, Acc}
    end.

%% @private collect the results from process parsing the examples
collect_parse_example_processes(_, 0, {_, Acc}) ->
    {ok, Acc};
collect_parse_example_processes(Self, Cores, {Reduce, Acc}) ->
    receive
        {done, Self, Part} ->
            NewAcc = Reduce(Part, Acc),
            collect_parse_example_processes(Self, Cores - 1, {Reduce, NewAcc});
        {error, Self, Reason} ->
            {error, Reason}
    end.

%% @private insert feature values into database
insert_features(Database, Attributes) ->
    ets:insert(Database#database.examples, list_to_tuple(Attributes)),
    ok.

%% @private format example values according to their correct type
parse_example_attributes([], [], _, Acc) ->
    lists:reverse(Acc);
parse_example_attributes([Value|Values], [Type|Types], Column, Acc) ->
    FeatureValue = case format_attribute_value(Type, Value) of
                       '?' -> '?';
                       error -> throw({error, {invalid_number_format, Column, Value}});
                       FormattedValue -> FormattedValue
                   end,
    parse_example_attributes(Values, Types, Column + 1, [FeatureValue|Acc]).

%% @doc determine if a string is a number or missing (?)
format_attribute_value(_, "?") ->
    '?';
format_attribute_value(numeric, Value) ->
    L = if is_binary(Value) -> binary_to_list(Value); true -> Value end,
    Float = (catch erlang:list_to_float(L)),
    case is_number(Float) of
        true -> Float;
        false ->
            Int = (catch erlang:list_to_integer(L)),
            case is_number(Int) of
                true -> Int;
                false ->
                    error
            end
    end;
format_attribute_value(categoric, Value) ->
    list_to_binary(Value).

%% @private pick element at N and return {PickedElement, Rest}
-spec cherrypick([any(), ...], integer()) -> {any(), [any(), ...]}.
cherrypick(List, N) ->
    {L1, [Item|L2]} = lists:split(N - 1, List),
    {list_to_atom(Item), L1 ++ L2}.

-ifdef(TEST).

%% write tests

-endif.    
    
    
    
