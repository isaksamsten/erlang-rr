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
	 load/2,
	 insert_prediction/2]).


insert_prediction(ExId, Pred) ->
    ets:insert(predictions, {exid(ExId), Pred}).

%%
%% Init an ets table that stores all examples in memory. The examples
%% are described with their features as a tuple. That is, {Id,
%% {x1,...,xn}}.
%%
init() ->
    ets:new(examples, [named_table, public, {read_concurrency, true}]),
    ets:new(features, [named_table, public]),
    ets:new(predictions, [named_table, public]).

%%
%% Load "File" using "Cores"
%%
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

    
%%
%% Spawns "Cores" 'parse_example_process' and collects their results
%%
parse_examples(File, Cores, ClassId, Types) ->
    Self = self(),
    lists:foreach(fun (_) ->
			  spawn_link(?MODULE, parse_example_process, [Self, File, ClassId, Types, dict:new()])
		  end, lists:seq(1, Cores)),
    collect_parse_example_processes(Self, Cores, dict:new()).

%%
%% Process that gets a line from the "File" and process each example
%%
parse_example_process(Parent, File, ClassId, Types, Acc) ->
    case csv:next_line(File) of
	{ok, Example, Id0} ->
	    {Class, Attributes} = take_feature(Example, ClassId),
%	    {Id1, Attributes0} = take_feature(Attributes, IdId),
	    Id = Id0 - 2, %% NOTE: subtracting headers 
	    ets:insert(examples, format_features(Attributes, Types, 1, [Id])),
	    parse_example_process(Parent, File, ClassId, Types, update_class_distribution(Class, Id, Acc));
	eof ->
	    Parent ! {done, Parent, Acc}
    end.

%%
%% Collect the results from process parsing the examples
%%
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
						    '?' ->
							'?';
						    false ->
							throw({error, {invalid_number_format, Column, Value}})
						end|Acc]).

%% Determine if a string is a number, or missing (?)
%% returns {true, int()|float()} or missing or false
format_number("?") ->
    '?';
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

format_class_distribution(Examples) ->
    lists:keysort(1, lists:map(fun ({Class, {Count, Ids}}) ->
				       {Class, Count, Ids}
			       end, dict:to_list(Examples))).

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

%%
%% Parse feature declaration
%%
parse_feature_declaration(Features0, ClassId, Types) ->
    {_, Features} = take_feature(Features0, ClassId),
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

format_split_distribution(Acc) ->
    lists:reverse(lists:foldl(fun({Value, Examples}, Result) ->
				      case dict:size(Examples) of
					  0 ->
					      Acc;
					  _ ->
					      [{Value, format_class_distribution(Examples)}|Result]
				      end
			      end, [], dict:to_list(Acc))).

%%
%% Format the Left and Right branch
%% TODO: rewrite to allow for sending a tuple instead...
%%
format_left_right_split([], Right) ->
    {right, Right};
format_left_right_split(Left, []) ->
    {left, Left};
format_left_right_split(Left, Right) ->
    {both, Left, Right}.

%%
%% Distribute missing values over the left and right branch using
%% "Distribute"
%%
distribute_missing_values(_, _, _, _, [], [], [], Left, Right, _) ->
    format_left_right_split(Left, Right);
distribute_missing_values(Feature, Examples, TotalNoLeft, TotalNoRight, [Left|LeftRest], [Right|RightRest], 
			  [{_, _, Missing}|MissingRest], LeftAcc, RightAcc, Distribute) ->
    case  distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, Missing, Left, Right, Distribute) of
	{{_, 0, []}, NewRight} ->
	    distribute_missing_values(Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest,
				      LeftAcc, [NewRight|RightAcc], Distribute);
	{NewLeft, {_, 0, []}} ->
	    distribute_missing_values(Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest, 
				      [NewLeft|LeftAcc], RightAcc, Distribute);
	{NewLeft, NewRight} ->
	    distribute_missing_values(Feature, Examples, TotalNoLeft, TotalNoRight, LeftRest, RightRest, MissingRest, 
				      [NewLeft|LeftAcc], [NewRight|RightAcc], Distribute)
    end.
	    
distribute_missing_values_for_class(_, _, _, _, [], Left, Right, _) ->
    {Left, Right};
distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, [MissingEx|RestMissing], 
				   {Class, NoLeft, Left} = LeftExamples, 
				   {Class, NoRight, Right} = RightExamples, Distribute) ->

    %% If distribute return true, missing values are distribute to the
    %% left, otherwise they are distributed to the right
    case Distribute(build, Feature, MissingEx, TotalNoLeft, TotalNoRight) of
	{right, {_, NewCount}=NewEx} ->
	    distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, LeftExamples,
						{Class, NoRight + NewCount, [NewEx|Right]}, Distribute);
	{left, {_, NewCount}=NewEx} ->
	    distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, 
						{Class, NoLeft + NewCount, [NewEx|Left]}, RightExamples, Distribute);
	{both, {{_, NewLeftCount}=NewLeftEx, {_, NewRightCount} = NewRightEx}} ->
	    distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing,
						{Class, NoLeft + NewLeftCount, [NewLeftEx|Left]},
						{Class, NoRight + NewRightCount, [NewRightEx|Right]}, Distribute);
	ignore ->
	    distribute_missing_values_for_class(Feature, Examples, TotalNoLeft, TotalNoRight, RestMissing, 
						LeftExamples, RightExamples, Distribute)
    end.

%%
%% Split "Examples" into two disjoint subsets according to "Feature".
%%  Left Branch: Less than and equal to, or, equal to
%%  Right branch: Greater than, or, not equal to
%%
split(Feature, Examples, Distribute, DistributeMissing) ->
    {Value, {Left, Right, Missing}} = split_with_value(Feature, Examples, Distribute),
    {Value, distribute_missing_values({Feature, Value}, Examples, count(Left), count(Right), 
				      Left, Right, Missing, [], [], DistributeMissing)}.


%%
%% Split into three disjoint subsets, Left, Right and those examples
%% having a missing value for "Feature"
%%
split_with_value(Feature, Examples, Distribute) when element(1, Feature) == numeric;
						     element(1, Feature) == categoric;
						     element(1, Feature) == combined->
    Value = sample_split_value(Feature, Examples),
    {Value, split_feature({Feature, Value}, Examples, Distribute, [], [], [])};
split_with_value(Feature, Examples, Distribute) ->
    {none, split_feature(Feature, Examples, Distribute, [], [], [])}.



%%
%% Split the class distribution:
%%  NOTE: LEFT is >= or == and RIGHT is < or /= 
%%
split_class_distribution(_, [], _, _, Left, Right, Missing) ->
    {Left, Right, Missing};
split_class_distribution(Feature, [ExampleId|Examples], Distribute, Class, 
			 {Class, NoLeft, Left} = LeftExamples, 
			 {Class, NoRight, Right} = RightExamples,
			 {Class, NoMissing, Missing} = MissingExamples) ->
    {NewLeftExamples, NewRightExamples, NewMissingExamples} = 
	case Distribute(Feature, ExampleId) of
	    {'?', Count} ->
		{LeftExamples, RightExamples, {Class, NoMissing + Count, [ExampleId|Missing]}};
	    {left, Count} ->
		{{Class, NoLeft + Count, [ExampleId|Left]}, RightExamples, MissingExamples};
	    {right, Count} ->
		{LeftExamples, {Class, NoRight + Count, [ExampleId|Right]}, MissingExamples};
	    {left, {_, NewNo} = NewEx, {_, NewNoMissing} = NewMissingEx} ->
		{{Class, NoLeft + NewNo, [NewEx|Left]}, RightExamples, {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
	    {right, {_, NewNo} = NewEx, {_, NewNoMissing} = NewMissingEx} ->
		{LeftExamples, {Class, NoLeft + NewNo, [NewEx|Right]}, {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
	    {all, {_, NewNoLeft} = NewLeftEx, {_, NewNoRight} = NewRightEx, {_, NewNoMissing} = NewMissingEx} ->
		{{Class, NoLeft + NewNoLeft, [NewLeftEx|Left]}, 
		 {Class, NoRight + NewNoRight, [NewRightEx|Right]},
		 {Class, NoMissing + NewNoMissing, [NewMissingEx|Missing]}};
	    {both, {_, NewNoLeft} = NewLeftEx, {_, NewNoRight} = NewRightEx} ->
		{{Class, NoLeft + NewNoLeft, [NewLeftEx|Left]},
		 {Class, NoRight + NewNoRight, [NewRightEx|Right]},
		 MissingExamples}
	end,
    split_class_distribution(Feature, Examples, Distribute, Class, NewLeftExamples, NewRightExamples, NewMissingExamples).

distribute({{categoric, FeatureId}, SplitValue}, ExId) ->
    {case feature(ExId, FeatureId) of
	'?' -> '?';
	Value when Value == SplitValue -> left;
	_ -> right
    end, count(ExId)};
distribute({{numeric, FeatureId}, Threshold}, ExId) ->
    {case feature(ExId, FeatureId) of
	'?' -> '?';
	Value when Value >= Threshold -> left;
	_ -> right
    end, count(ExId)};
distribute({{combined, FeatureA, FeatureB}, {combined, SplitValueA, SplitValueB}}, ExId) ->
    {A, _} = distribute({FeatureA, SplitValueA}, ExId),
    {B, C} = distribute({FeatureB, SplitValueB}, ExId),
    {case {A, B} of
	{'?', B} ->
	    B;
	{A, '?'} ->
	    A;
	 {'?', '?'} ->
	     '?';
	{A, B} when A == B ->
	    A;
	{A, B} when A =/= B ->
	    Rand = random:uniform(),
	    if Rand >= 0.5 ->
		    A;
	       true ->
		    B
	    end
     end, C};
distribute({rule, Rule, _Lenght}, ExId) ->
    %% Distribute all examples for which the rules hold to the left
    %% RuleConjunction = [rule()...] where rule() = {feature(), Value}
    %% if [] -> all is true
    %% NOTE: could we do this by sending a fraction (the number of
    %% rules that apply) to the left and the rest to the right?
    {rr_rule:evaluate_rule(Rule, ExId), count(ExId)}.

evaluate_rule([], _) ->
    left;
evaluate_rule([Rule|Rest], ExId) ->
    case distribute(Rule, ExId) of
	{left, _} ->
	    evaluate_rule(Rest, ExId);
	{right, _} ->
	    right;
	{'?', _} ->
	    '?'
    end.


split_feature(_Feature, [], _, Left, Right, Missing) ->
    {Left, Right, Missing};
split_feature(Feature, [{Class, _, ExampleIds}|Examples], Distribute, Left, Right, Missing) ->
    case split_class_distribution(Feature, ExampleIds, Distribute, Class, {Class, 0, []}, {Class, 0, []}, {Class, 0, []}) of
	{LeftSplit, RightSplit, MissingSplit} ->
	    split_feature(Feature, Examples, Distribute, [LeftSplit|Left], [RightSplit|Right], [MissingSplit|Missing])
    end.

%%
%% Find the best numeric split point. TODO: handle missing values
%%
deterministic_numeric_split(FeatureId, Examples, Gain) ->
    [{Value, Class}|ClassIds] = lists:keysort(1, lists:foldl(
					  fun ({Class, _, ExIds}, NewIds) ->
						  lists:foldl(fun(ExId, Acc) ->
								      [{feature(ExId, FeatureId), Class}|Acc]
							      end, NewIds, ExIds)
					  end, [], Examples)),
    
    Gt = lists:map(fun({C, Num, _}) -> {C, Num, []} end, Examples),
    Lt = lists:map(fun({C, _, _}) -> {C, 0, []} end, Examples),
    Dist = [Lt, Gt],
    First = {Value, Class},
    Total = rr_example:count(Examples),
    deterministic_numeric_split(ClassIds, First, FeatureId, Gain, Total, {Value/2, inf}, Dist).

deterministic_numeric_split([], _, _, _, _, {Threshold, _}, _) ->
    Threshold;
deterministic_numeric_split([{Value, Class}|Rest], {OldValue, OldClass}, FeatureId, 
			    Gain, Total, {OldThreshold, OldGain}, Dist) ->

    [Left, Right] = Dist, 
    Dist0 = case lists:keytake(Class, 1, Left) of
		{value, {Class, Num, _}, ClassRest} ->
		    [[{Class, Num + 1, []}|ClassRest], Right]
	    end,
    [Left0, Right0] = Dist0,
    NewDist = case lists:keytake(Class, 1, Right0) of
	{value, {Class, Num0, _}, ClassRest0} ->
	    [Left0, [{Class, Num0 - 1, []}|ClassRest0]]
    end,
    case Class == OldClass of
	true -> deterministic_numeric_split(Rest, {Value, Class}, FeatureId,
					    Gain, Total, {OldThreshold, OldGain}, NewDist);
	false ->
	    Threshold = (Value + OldValue) / 2,
	    NewGain0 = Gain(NewDist, Total),
	    NewThreshold = case NewGain0 < OldGain of
			       true -> {Threshold, NewGain0};
			       false -> {OldThreshold, OldGain}
			   end,
	    deterministic_numeric_split(Rest, {Value, Class}, FeatureId,
					Gain, Total, NewThreshold, NewDist)
    end.

sample_split_value(Feature, Examples) ->
    case Feature of
	{categoric, FeatureId} ->
	    resample_random_split(FeatureId, Examples, 5);
	{numeric, FeatureId} ->
	    random_numeric_split(FeatureId, Examples);
	{combined, A, B} ->
	    sample_combined(A, B, Examples)
    end.

sample_split_value(Feature, Examples, Ex1, Ex2) ->
    case Feature of
	{categoric, FeatureId} ->
	    feature(Ex1, FeatureId);
	{numeric, FeatureId} ->
	    case random_numeric_split(FeatureId, Ex1, Ex2) of
		{'?', '?'} ->
		    0;
		X ->
		    X
	    end;
	{combined, A, B} ->
	    sample_combined(A, B, Examples)
    end.

%%
%% Sample these from ONE example
%%
sample_combined(FeatureA, FeatureB, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    {combined, sample_split_value(FeatureA, Examples, Ex1, Ex2), sample_split_value(FeatureB, Examples, Ex1, Ex2)}.

random_numeric_split(FeatureId, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    case random_numeric_split(FeatureId, Ex1, Ex2) of
	{'?', '?'} ->
	    random_numeric_split(FeatureId, Examples);
	X ->
	    X
    end.

random_numeric_split(FeatureId, Ex1, Ex2) ->
    Value1 = feature(Ex1, FeatureId),
    Value2 = feature(Ex2, FeatureId),
    case {Value1, Value2} of
	{'?', Value2} ->
	    Value2;
	{Value1, '?'} ->
	    Value1;
	{Value1, Value2} ->
	    (Value1 + Value2) / 2;
	{'?', '?'} ->
	    {'?', '?'}
    end.

resample_random_split(_, _, 0) ->
    '?';
resample_random_split(FeatureId, Examples, N) ->
    case random_categoric_split(FeatureId, Examples) of	
	'?' ->
	    resample_random_split(FeatureId, Examples, N - 1);
	X ->  
	    X
    end.

random_categoric_split(FeatureId, Examples) ->
    ExId = sample_example(Examples),
    feature(ExId, FeatureId).

%%
%% Take class at id=N and return the the tuple {Class, Classes}
%%
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

clone(ExId) ->
    {exid(ExId), exid(ExId)}.

exid(ExId) when is_number(ExId) ->
    ExId;
exid({ExId, _}) when is_number(ExId) ->
    ExId.

%%
%% Count the occurences of "Class" in "Examples"
%%
count(Class, Examples) ->
    case lists:keysearch(Class, 1, Examples) of
	{value, {_, N, _}} ->
	    N;
	_ -> 
	    0
    end.

majority(Examples) ->
    {Class, Count, _} = lists:foldl(fun({Class, Count, _}, {_OldClass, OldCount, _} = Old) ->
					    case Count >= OldCount of 
						true -> {Class, Count, []};
						false -> Old
					    end
				    end, hd(Examples), tl(Examples)),
    {Class, Count}.

get_class(Class, Examples) ->
    lists:keyfind(Class, 1, Examples).

classes(Examples) ->
    length(Examples).

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
to_binary(Positive, Examples) ->
    case lists:keytake(Positive, 1, Examples) of
	{value, {_, Pc, Positives}, Negatives0} ->
	    [{'+', Pc, Positives}, lists:foldl(fun({_, Nc, Ids}, {_, N, Acc}) ->
						       {'-', Nc+N, Acc ++ Ids}
					       end, {'-', 0, []}, Negatives0)];
	false ->
	    throw({error, cannot_split})
    end.

%%
%% Remove Examples from "Examples" that are covered by "Covered"
%%
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

%%
%% Return a tuple {Pos, Neg} with the number of Positive and negative examples
%% covered by "Example"
%%
coverage(Examples) ->
    {rr_example:count('+', Examples), rr_example:count('-', Examples)}.


feature(ExId, At) ->
    ets:lookup_element(examples, exid(ExId), At + 1).


feature_id({{combined, IdA, IdB}, _}) ->
    list_to_tuple(lists:sort([feature_id(IdA), feature_id(IdB)]));
feature_id({{_, Id}, _}) ->
    Id;
feature_id({rule, Rules, _Length}) ->
    Ids = [feature_id(Rule) || Rule <- Rules],
    lists:sort(Ids);
feature_id({_, Id}) ->
    Id.

feature_name({IdA, IdB}) ->
    {feature_name(IdA),
     feature_name(IdB)};
feature_name(Rules) when is_list(Rules) ->
    [feature_name(Rule) || Rule <- Rules];
feature_name(Id) ->
    ets:lookup_element(features, Id, 2).





%%
%% Generate a set of random numbers
%%
generate_featurestrap(Features, Subset) ->
    Length = length(Features),
    case Length >= Subset of
	true -> generate_featurestrap(Subset, Length, sets:new());
	false -> lists:seq(1, Length) 
    end.
generate_featurestrap(N, Length, Set) ->
    Random = random:uniform(Length),
    Set0 = sets:add_element(Random, Set),
    case sets:size(Set0) == N of
	true ->
	    Set0;
	false ->
	    generate_featurestrap(N, Length, Set0)
    end.
	    
%%
%% Return a random subset of size "Subset" from Features
%%						    
random_features(Features, Subset) ->
    Strap = generate_featurestrap(Features, Subset),
    {_, F} = lists:foldl(fun({Type, Id}, {Index, Acc}) ->
				 case sets:is_element(Index, Strap) of
				     true -> {Index + 1, [{Type, Id}|Acc]};
				     false -> {Index + 1, Acc}
				 end
			 end, {1, []}, Features),
    F.

%%
%% Return the dataset splitted into {Train, Test} with "Ratio"
%% examples belonging to Train
%% 
split_dataset(Examples, Ratio) ->
    lists:foldl(fun({Class, Count, Ids}, 
		    {TrainAcc, TestAcc}) ->
			{Train, Test} = lists:split(round(Count * Ratio), Ids),
			
			{[{Class, length(Train), Train}|TrainAcc],
			 [{Class, length(Test), Test}|TestAcc]}
		end, {[], []}, Examples).

shuffle_dataset(Examples) ->
    lists:foldl(fun({Class, Count, Ids}, Acc) ->
			[{Class, Count, shuffle_list(Ids)}|Acc]
		end, [], Examples).

shuffle_list(Ids0) ->
    [Id || {_, Id} <- lists:keysort(1, lists:map(fun (Id) -> 
							 {random:uniform(), Id} 
						 end, Ids0))].
								 
%%
%% TODO: make it stratified
%%
cross_validation(Fun, Folds, Examples) ->
    NoExamples = count(Examples),
    ExampleIds = [Id || {_, Id} <- lists:keysort(1, lists:map(fun(Id) -> {random:uniform(), Id} end, 
							      lists:seq(1, NoExamples)))],
    cross_validation(Fun, ExampleIds, Examples, Folds, Folds, NoExamples, []).

cross_validation(_, _, _, _, 0, _, Acc) -> 
    lists:reverse(Acc);
cross_validation(Fun, ExampleIds, Examples, Folds, CurrentFold, NoExamples, Acc) -> 
    {Test, Train} = lists:split(round(NoExamples * 1/Folds), ExampleIds),
    
    Test0 = get_examples_with(Test, Examples),
    Train0 = get_examples_with(Train, Examples),

    Result = Fun(Train0, Test0, Folds - CurrentFold + 1),
    cross_validation(Fun, Train ++ Test, Examples, Folds, CurrentFold - 1, NoExamples, [Result|Acc]). 

get_examples_with(Ids, Examples) ->
    get_examples_with(gb_sets:from_list(Ids), Examples, []).

get_examples_with(_, [], Acc) ->
    Acc;    
get_examples_with(Ids, [{Class, _Count, ExIds}|Rest], Acc) ->
    get_examples_with(Ids, Rest, [get_examples_with_for_class(Ids, Class, ExIds, [])|Acc]).

get_examples_with_for_class(_, Class, [], Acc) ->
    {Class, length(Acc), Acc};
get_examples_with_for_class(Ids, Class, [ExId|Rest], Acc) ->
    case gb_sets:is_element(ExId, Ids) of
	true ->
	    get_examples_with_for_class(Ids, Class, Rest, [ExId|Acc]);
	false ->
	    get_examples_with_for_class(Ids, Class, Rest, Acc)
    end.

%%
%% Generate a bootstrap replicate of "Examples" with {InBag, OutOfBag}
%% examples.
%%
bootstrap_aggregate(Examples) ->
    MaxId = count(Examples),
    Bootstrap = generate_bootstrap(MaxId),
    select_bootstrap_examples(Examples, 1, Bootstrap, {[], []}).

generate_bootstrap(MaxId) ->
    lists:foldl(fun(_, Bootstrap) ->
			dict:update(random:uniform(MaxId), fun (Count) -> Count + 1 end, 1, Bootstrap)
		end, dict:new(), lists:seq(1, MaxId)).


subset_aggregate(Examples) ->
    MaxId = count(Examples),
    {Bootstrap, _Ignore} = lists:split(MaxId div 2, generate_substrap(MaxId)),
    select_bootstrap_examples(Examples, 1, lists:foldl(fun(N, D) -> dict:store(N, 1, D) end, dict:new(), Bootstrap), {[], []}).

generate_substrap(MaxId) ->
    shuffle_list(lists:seq(1, MaxId)).
    

select_bootstrap_examples([], _N, _Bootstrap, Acc) ->
    Acc;
select_bootstrap_examples([{Class, Count, Ids}|Examples], N, Bootstrap, {InBags, OutBags}) ->
    case select_bootstrap_examples_for_class(Class, {0, 0}, N, Ids, Bootstrap, {[], []}) of
	{{_, 0, []}, _} ->
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, {InBags, OutBags});
	{InBag, OutBag} ->
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, {[InBag|InBags], [OutBag|OutBags]})
    end.

select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, _N, [], _, {InBag, OutBag}) ->
    {{Class, InBagCount, InBag}, {Class, OutBagCount, OutBag}};
select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, N, [ExId|Rest], Bootstrap, {InBag, OutBag}) ->
    case dict:find(N, Bootstrap) of
	{ok, Times} ->
	    NewInBag = duplicate_example(ExId, Times, InBag), %% NOTE: could be changed to "Times"
	    select_bootstrap_examples_for_class(Class, {InBagCount + Times,  OutBagCount},
						N+1, Rest, Bootstrap, {NewInBag, OutBag});
	error ->
	    select_bootstrap_examples_for_class(Class, {InBagCount,  OutBagCount + 1},
						N+1, Rest, Bootstrap, {InBag, [ExId|OutBag]})
    end.

duplicate_example({ExId, _}, N, Acc) ->
    [{ExId, N}|Acc];
duplicate_example(ExId, N, Acc) ->
    [{ExId, N}|Acc].

sample_example([{_Class, _, ExIds}]) ->
    lists:nth(random:uniform(length(ExIds)), ExIds);
sample_example(Examples) ->
    sample_example([lists:nth(random:uniform(length(Examples)), Examples)]).

sample_example_pair([{_, _, ExId1}, {_, _, ExId2}]) ->
    sample_example_pair(ExId1, ExId2);
sample_example_pair(Examples) ->
    sample_example_pair(sample_class_pair(Examples)).

sample_example_pair(ExId1, ExId2) ->
    {lists:nth(random:uniform(length(ExId1)), ExId1),
     lists:nth(random:uniform(length(ExId2)), ExId2)}.

sample_class_pair(Examples) ->
    NoEx = length(Examples),
    Random = random:uniform(NoEx),
    sample_class_pair(Examples, Random, NoEx, [lists:nth(Random, Examples)]).

sample_class_pair(Examples, Random, NoEx, Acc) ->
    case random:uniform(NoEx) of
	Random ->
	    sample_class_pair(Examples, Random, NoEx, Acc);
	Other ->
	    [lists:nth(Other, Examples)|Acc]
    end.
