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
-export([init/0,
	 load/2]).


%%
%% Init an ets table that stores all examples in memory. The examples
%% are described with their features as a tuple. That is, {Id,
%% {x1,...,xn}}.
%%
init() ->
    ets:new(examples, [named_table, public, {read_concurrency, true}]),
    ets:new(features, [named_table, public]).

load(File, Cores) ->
    {ClassId, Types} = case csv:next_line(File) of
			   {ok, Types0, _} ->
			       parse_type_declaration(Types0);
			   eof ->
			       throw({error, features_type_error})
		       end,
    Features = case csv:next_line(File) of
		   {ok, Features0, _} ->
		       parse_feature_declaration(Features0, ClassId, Types);
		   eof ->
		       throw({error, features_type_error})
	       end,
    {Features, parse_examples(File, Cores, ClassId, Types)}.

    
parse_examples(File, Cores, ClassId, Types) ->
    Self = self(),
    lists:foreach(fun (_) ->
			  spawn_link(?MODULE, parse_example_process, [Self, File, ClassId, Types, dict:new()])
		  end, lists:seq(1, Cores)),
    collect_parse_example_processes(Self, Cores, dict:new()).

parse_example_process(Parent, File, ClassId, Types, Acc) ->
    case csv:next_line(File) of
	{ok, Example, Id} ->
	    {Class, Attributes} = take_class(Example, ClassId),
	    ets:insert(examples, {Id, format_features(Attributes, Types, 1, [])}),
	    parse_example_process(Parent, File, ClassId, Types, update_class_distribution(Class, Id, Acc));
	eof ->
	    Parent ! {done, Parent, Acc}
    end.

%%
%% Collect the results from process parsing the examples
%%
collect_parse_example_processes(_, 0, Examples) ->
    lists:map(fun ({Class, {Count, Ids}}) ->
		      {Class, Count, Ids}
	      end, dict:to_list(Examples));
collect_parse_example_processes(Self, Cores, Examples) ->
    receive
	{done, Self, Part} ->
	    collect_parse_example_processes(Self, Cores - 1, 
					    dict:merge(fun (_, {CountA, IdsA}, {CountB, IdsB}) ->
							       {CountA + CountB, IdsA ++ IdsB}
						       end, Examples, Part))
    end.

%%
%% Format example values according to their correct type
%%
format_features([], [], _, Acc) ->
    list_to_tuple(lists:reverse(Acc));
format_features([Value|Values], [categoric|Types], Column, Acc) ->
    format_features(Values, Types, Column + 1, [list_to_atom(Value)|Acc]);
format_features([Value|Values], [numeric|Types], Column, Acc) ->
    format_features(Values, Types, Column + 1, [case format_number(Value) of
						    {true, Number} ->
							Number;
						    missing ->
							0;
						    false ->
							throw({error, {invalid_number_format, Column, Value}})
						end|Acc]).

%% Determine if a string is a number, or missing (?)
%% returns {true, int()|float()} or missing or false
format_number("?") ->
    missing;
format_number(L) ->
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



%%
%% Merge two dictionaries with class distributions
%%
update_class_distribution(Class, Id, Acc) ->
    dict:update(Class, fun({Count, Ids}) ->
			       {Count + 1, [Id|Ids]}
		       end, {1, [Id]}, Acc).
    

    
%%
%% Parses a type declaration: ["class", "categoric"+, "numeric"+] in
%% any order. Returns {ClassId, [features...]}
%%
parse_type_declaration(Types) ->
    parse_type_declaration(Types, missing, 1, []).

parse_type_declaration([], ClassId, _, Acc) ->
    {ClassId, lists:reverse(Acc)};
parse_type_declaration([Type0|Rest], ClassId, Id, Acc) ->
    Type = list_to_atom(string:to_lower(Type0)),
    case Type of
	Type when Type =:= numeric;
		  Type =:= categoric ->
	    parse_type_declaration(Rest, ClassId, Id + 1, [Type|Acc]);
	Type when Type =:= class;
		  ClassId =:= missing ->
	    parse_type_declaration(Rest, Id, Id + 1, Acc);
	_ ->
	    throw({error, {invalid_type_declaration, Id}})
    end.

%%
%% Parse feature declaration
%%
parse_feature_declaration(Features0, ClassId, Types) ->
    {_, Features} = take_class(Features0, ClassId),
    if length(Features) =/= length(Types) ->
	    throw({error, {invalid_feature_declaration, {length(Features), '/=', length(Types)}}});
       true ->
	    parse_feature_declaration(Features, Types, 1, [])
    end.

parse_feature_declaration([], [], _, Acc) ->
    lists:reverse(Acc);
parse_feature_declaration([Feature|Features], [Type|Types], Id, Acc) ->
    ets:insert(features, {Id, Feature}),
    parse_feature_declaration(Features, Types, Id + 1, [{Type, Id}|Acc]).


split(Feature, Examples) ->
    split(Feature, Examples, dict:new()).

split(_, [], Acc) ->
    Acc;
split({categoric, _} = Feature, [{Class, _, ExampleIds}|Examples], Acc) ->
    split(Feature, Examples, split_class_distribution(Feature, ExampleIds, Class, Acc));
split({numeric, FeatureId} = Feature, Examples, Acc) ->
    Threshold = random_numeric_split(FeatureId, Examples),
    split_numeric_feature(Feature, Threshold, Examples, Acc).
	

split_class_distribution(_, [], _, Dict) ->
    Dict;
split_class_distribution({categoric, FeatureId} = Feature, [ExampleId|Examples], Class, Dict) ->
    Value = get_feature(ExampleId, FeatureId),
    split_class_distribution(Feature, Examples, Class,
			     dict:update(Value, fun(Classes) ->
							dict:update(Class, fun ({Count, Ids}) ->
										   {Count + 1, [ExampleId|Ids]}
									   end, {1, [ExampleId]}, Classes)
						end, dict:new(), Dict));
split_class_distribution({{numeric, FeatureId}, Threshold} = Feature, [ExampleId|Examples], Class, Dict) ->
    Value = get_feature(ExampleId, FeatureId),
    split_class_distribution(Feature, Examples, Class, Dict).

%%
%% Split a numeric feature at threshold
%%
split_numeric_feature(_, Threshold, [], Acc) ->
    {Threshold, Acc};
split_numeric_feature(Feature, Threshold, [{Class, _, ExampleIds}|Examples], Acc) ->
    split_numeric_feature(Feature, Threshold, Examples,
			  split_class_distribution({Feature, Threshold}, ExampleIds, Class, Acc)).


random_numeric_split(_, _) ->
    5.
%%
%% Take class at id=N and return the the tuple {Class, RestOfList}
%%
take_class([A|R], 1) ->
    {A, R};
take_class(List, N) ->
    {L1, [Item|L2]} = lists:split(N - 1, List),
    {Item, L1 ++ L2}.

%%
%% Count the number of examples in "Examples"
%%
count(Examples) ->
    lists:foldl(fun({_, Count, _}, Old) ->
			Count + Old
		end, 0, Examples).

%%
%% Count the occurences of "Class" in "Examples"
%%
count(Class, Examples) ->
    case lists:keysearch(Class, 1, Examples) of
	{value, {_, N, _}} ->
	    N;
	_ -> throw({error, no_such_class})
    end.

%%
%% Count the number of examples in "Examples" excluding examples with
%% class Class
%%
count_exclude(Class, Examples) ->
    lists:foldl(fun({Cls, _, _}, Old) when Class =:= Cls->
			Old;
		   ({_, Count, _}, Old) ->
			Old + Count
		end, 0, Examples).

%%
%% Transform the examples into a form where we have a set of positive
%% and a set of negative examples
%%
%% Returns: {{NumberOfPositive, [IdsOfPositive...]}, 
%%           {NumberOfNegative, [IdsOfNegative...]}}
%%
get_positive_negative(Positive, Examples) ->
    case lists:keytake(Positive, 1, Examples) of
	{value, {_, Pc, Positives}, Negatives0} ->
	    [{'+', Pc, Positives}, lists:foldl(fun({_, Nc, Ids}, {_, N, Acc}) ->
						       {'-', Nc+N, Acc ++ Ids}
					       end, {'-', 0, []}, Negatives0)];
	false ->
	    throw({error, cannot_split})
    end.

%%
%% Get the feature vector for example with "Id"
%%
get_example(Id) ->
    [{_, Value}|_] = ets:lookup(examples, Id),
    Value.

%%
%% Get feature at index "At" from "Id"
%%
get_feature(Id, At) ->
    element(At, get_example(Id)).

test() ->
    rr_example:init(),
    File = csv:reader("../data/car.txt"),
    {Features, Examples} = load(File, 4),
    {Features, Examples}.
   
    

