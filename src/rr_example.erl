%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% Module for handling the examples
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr_example).

-export([
	 init/0,
	 load/2,
	 insert_prediction/2,
	 get_prediction/1,

	 format_number/1,

	 sample_split_value/2,
	 find_numeric_split/3,
	 sample_example_pair/1,
	 
	 split/4,
	 split/5,
	 distribute/2,
	 
	 exid/1,
	 count/2,
	 count/1,
	 clone/1,

	 shuffle/1,
	 flatten/1,
	 random_features/2,
	 unpack_split/1,

	 vector/1,
	 feature/2,
	 feature_id/1,
	 feature_name/1,

	 to_binary/2,
	 count_exclude/2,
	 remove_covered/2,
	 coverage/1,

	 get_class/2,
	 classes/1,
	 majority/1,
	 randomize/1,
	 cross_validation/3,
	 split_dataset/2,

	 bootstrap_aggregate/1,
	 subset_aggregate/1,

	 parse_example_process/5,

	 best_split/7,
	 best_split/8
	]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @headerfile "rr.hrl"
-include("rr.hrl").

%% @doc insert a prediction into the global table of all predictions
-spec insert_prediction(exid(), any()) -> ok.
insert_prediction(ExId, Pred) ->
    ets:insert(predictions, {exid(ExId), Pred}).

get_prediction(ExId) ->
    hd(ets:lookup(predictions, exid(ExId))).

%% @doc init ets-tables storing features, examples and predictions
-spec init() -> ok.
init() ->
    ets:new(examples, [named_table, public, {read_concurrency, true}]),
    ets:new(features, [named_table, public]),
    ets:new(predictions, [named_table, public]).

-spec load(string(), number()) -> {features(), examples()}.
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

    
%% @private spawns "Cores" 'parse_example_process' and collects their results
parse_examples(File, Cores, ClassId, Types) ->
    Self = self(),
    lists:foreach(fun (_) ->
			  spawn_link(?MODULE, parse_example_process, [Self, File, ClassId, Types, dict:new()])
		  end, lists:seq(1, Cores)),
    collect_parse_example_processes(Self, Cores, dict:new()).

%% @private process that gets a line from the "File" and process each example
parse_example_process(Parent, File, ClassId, Types, Acc) ->
    case csv:next_line(File) of
	{ok, Example, Id0} ->
	    {Class, Attributes} = take_feature(Example, ClassId),
	    Id = Id0 - 2, %% NOTE: subtracting headers 
	    ets:insert(examples, format_features(Attributes, Types, 1, [Id])),
	    parse_example_process(Parent, File, ClassId, Types, update_class_distribution(Class, Id, Acc));
	eof ->
	    Parent ! {done, Parent, Acc}
    end.

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

%% @doc determine if a string is a number or missing (?)
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

%% @private format the left and right distribution
-spec format_left_right_split([examples()], [examples()]) -> split().
format_left_right_split([], Right) ->
    {right, Right};
format_left_right_split(Left, []) ->
    {left, Left};
format_left_right_split(Left, Right) ->
    {both, Left, Right}.

%% @private distribute missing values either right or left depending on Distribute
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

%% @doc unpack a split to a tuple containing {Left, Right}
-spec unpack_split(split()) -> {examples() | [], examples() | []}.
unpack_split({both, Left, Right}) ->
    {Left, Right};
unpack_split({left, Left}) ->
    {Left, []};
unpack_split({right, Right}) ->
    {[], Right}.


%% @doc Split Examples into two disjoint subsets according to Feature.
-spec split(feature(), examples(), distribute_fun(), missing_fun(), any()) -> {none | atom(), split()}.
split(Feature, Examples, Distribute, DistributeMissing, Sample) ->
    {Value, {Left, Right, Missing}} = split_with_value(Feature, Examples, Distribute, Sample),
    {Value, distribute_missing_values({Feature, Value}, Examples, count(Left), count(Right), 
				      Left, Right, Missing, [], [], DistributeMissing)}.

%% @doc split examples into two subsets according to feature handle split randomly
-spec split(feature(), examples(), distribute_fun(), missing_fun()) -> {none | atom(), split()}.
split(Feature, Examples, Distribute, DistributeMissing) ->
    split(Feature, Examples, Distribute, DistributeMissing, fun sample_split_value/2).
    
%% @private Split into three disjoint subsets, Left, Right and Missing
split_with_value(Feature, Examples, Distribute, Sample) ->
    case Sample(Feature, Examples) of
	none -> 
	    {none, split_feature(Feature, Examples, Distribute, [], [], [])};
	Value ->
	    {Value, split_feature({Feature, Value}, Examples, Distribute, [], [], [])}
    end.

%% @private split the class distribution (i.e. one example())
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

%% @doc default function for distributing examples left or right
-spec distribute(Feature::feature(), exid()) -> distribute_example().
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
    {rf_rule:evaluate_rule(Rule, ExId), count(ExId)}.

%% @private split data set using Feature
split_feature(_Feature, [], _, Left, Right, Missing) ->
    {Left, Right, Missing};
split_feature(Feature, [{Class, _, ExampleIds}|Examples], Distribute, Left, Right, Missing) ->
    case split_class_distribution(Feature, ExampleIds, Distribute, Class, {Class, 0, []}, {Class, 0, []}, {Class, 0, []}) of
	{LeftSplit, RightSplit, MissingSplit} ->
	    split_feature(Feature, Examples, Distribute, [LeftSplit|Left], [RightSplit|Right], [MissingSplit|Missing])
    end.

%% @private find the best numeric split point
find_numeric_split(FeatureId, Examples, Gain) ->
    case lists:keysort(1, lists:foldl(
			    fun ({Class, _, ExIds}, NewIds) ->
						  lists:foldl(fun(ExId, Acc) ->
								      case feature(ExId, FeatureId) of
									  '?' -> Acc;
									  Feature -> [{Feature, Class}|Acc]
								      end
							      end, NewIds, ExIds)
			    end, [], Examples)) of
	[{Value, Class}|ClassIds] ->
	    Gt = lists:map(fun({C, Num, _}) -> {C, Num, []} end, Examples),
	    Lt = lists:map(fun({C, _, _}) -> {C, 0, []} end, Examples),
	    Dist = {both, Lt, Gt},
	    First = {Value, Class},
	    Total = rr_example:count(Examples),
	    find_numeric_split(ClassIds, First, FeatureId, Gain, Total, {Value/2, inf}, Dist);
	[] ->
	    0.0 %% All values were missing. We have to guess....
    end.

find_numeric_split([], _, _, _, _, {Threshold, _}, _) ->
    Threshold;
find_numeric_split([{Value, Class}|Rest], {OldValue, OldClass}, FeatureId, 
			    Gain, Total, {OldThreshold, OldGain}, Dist) ->

    {both, Left, Right} = Dist, 
    Dist0 = case lists:keytake(Class, 1, Left) of
		{value, {Class, Num, _}, ClassRest} ->
		    {both, [{Class, Num + 1, []}|ClassRest], Right}
	    end,
    {both, Left0, Right0} = Dist0,
    NewDist = case lists:keytake(Class, 1, Right0) of
	{value, {Class, Num0, _}, ClassRest0} ->
	    {both, Left0, [{Class, Num0 - 1, []}|ClassRest0]}
    end,
    case Class == OldClass of
	true -> find_numeric_split(Rest, {Value, Class}, FeatureId,
					    Gain, Total, {OldThreshold, OldGain}, NewDist);
	false ->
	    Threshold = (Value + OldValue) / 2,
	    {NewGain0, _, _} = Gain(NewDist, Total),
	    NewThreshold = case NewGain0 < OldGain of
			       true -> {Threshold, NewGain0};
			       false -> {OldThreshold, OldGain}
			   end,
	    find_numeric_split(Rest, {Value, Class}, FeatureId,
					Gain, Total, NewThreshold, NewDist)
    end.

%% @doc 
%% sample a split point. this function is used in split() and can be overriden. 
%% please use this as the default
%% @end
sample_split_value(Feature, Examples) ->
    case Feature of
	 {categoric, FeatureId} ->
	     resample_categoric_split(FeatureId, Examples, 5);
	 {numeric, FeatureId} ->
	     sample_numeric_split(FeatureId, Examples);
	 {combined, A, B} ->
	     sample_combined(A, B, Examples);
	 _ ->
	     none
     end.

sample_split_value(Feature, Examples, Ex1, Ex2) ->
    case Feature of
	{categoric, FeatureId} ->
	    feature(Ex1, FeatureId);
	{numeric, FeatureId} ->
	    case sample_numeric_split(FeatureId, Ex1, Ex2) of
		{'?', '?'} ->
		    0;
		X ->
		    X
	    end;
	{combined, A, B} ->
	    sample_combined(A, B, Examples)
    end.

%% @private sample two features from the same example
sample_combined(FeatureA, FeatureB, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    {combined, sample_split_value(FeatureA, Examples, Ex1, Ex2), sample_split_value(FeatureB, Examples, Ex1, Ex2)}.

%% @private sample a numeric split point
sample_numeric_split(FeatureId, Examples) ->
    {Ex1, Ex2} = sample_example_pair(Examples),
    case sample_numeric_split(FeatureId, Ex1, Ex2) of
	'?' ->
	   '?';
	X ->
	    X
    end.
sample_numeric_split(FeatureId, Ex1, Ex2) ->
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
	    '?'
    end.

%% @private resample a random categoric split if a missing value is found
resample_categoric_split(_, _, 0) ->
    '?';
resample_categoric_split(FeatureId, Examples, N) ->
    case sample_categoric_split(FeatureId, Examples) of	
	'?' ->
	    resample_categoric_split(FeatureId, Examples, N - 1);
	X ->  
	    X
    end.

%% @private sample a categoric split
sample_categoric_split(FeatureId, Examples) ->
    ExId = sample_example(Examples),
    feature(ExId, FeatureId).

%% @doc find the best split from features
best_split([], _, _, _, _, _, _) ->
    no_features;
best_split([F|Features], Examples, Total, Score, Split, Distribute, Missing) ->
    {T, ExSplit} = Split(F, Examples, Distribute, Missing),
    Cand = #rr_candidate{feature={F, T}, score=Score(ExSplit, Total), split=ExSplit},
    best_split(Features, Examples, Total, Score, Split, Distribute, Missing, Cand).

best_split([], _, _, _, _, _, _, Acc) ->
    Acc;
best_split([F|Features], Examples, Total, Score, Split, Distribute, Missing, OldCand) ->
    Cand = case Split(F, Examples, Distribute, Missing) of
	       {Threshold, ExSplit} ->
		   #rr_candidate{feature = {F, Threshold}, 
				 score = Score(ExSplit, Total), 
				 split = ExSplit}		       
	   end,
    best_split(Features, Examples, Total, Score, Split, Distribute, Missing, 
		   case Cand#rr_candidate.score < OldCand#rr_candidate.score of
		       true -> Cand;
		       false -> OldCand
		   end).

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
					    case Count > OldCount of 
						true -> {Class, Count, []};
						false -> Old
					    end
				    end, hd(Examples), tl(Examples)),
    {Class, Count}.

get_class(Class, Examples) ->
    lists:keyfind(Class, 1, Examples).

classes(Examples) ->
    length(Examples).

flatten(Examples) ->
    lists:foldl(fun ({_, _, Ex}, Acc) ->
			Ex ++ Acc
		end, [], Examples).

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

-spec feature(feature(), number()) -> ok.
feature(ExId, At) ->
    ets:lookup_element(examples, exid(ExId), At + 1).

vector(ExId) ->
    hd(ets:lookup(examples, exid(ExId))).


feature_id({{combined, IdA, IdB}, _}) ->
    list_to_tuple(lists:sort([feature_id(IdA), feature_id(IdB)]));
feature_id({{_, Id}, _}) ->
    Id;
feature_id({rule, {Rules, _}, _Length}) ->
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

%% @doc generate a dictionary of Folds folds.
-spec generate_folds(examples(), integer()) -> dict().
generate_folds(Examples, Folds) ->
    generate_folds(Examples, Folds, 1, dict:new()).

%% @private
generate_folds([], _, _, Acc) ->
    Acc;
generate_folds([{Class, _, ExIds}|Rest], Folds, CurrentFold, Acc) ->
    {ClassFolds, NewCurrentFold} = generate_folds_for_class(Class, ExIds, Folds, CurrentFold, 
							    make_default_folds(Folds, Class, dict:new())),
    NewAcc = dict:merge(fun (_, A, B) -> A ++ B end, ClassFolds, Acc),
    generate_folds(Rest, Folds, NewCurrentFold, NewAcc).

make_default_folds(0, _, Acc) ->
    Acc;
make_default_folds(Fold, Class, Acc) ->
    make_default_folds(Fold - 1, Class, dict:store(Fold, [{Class, 0, []}], Acc)).

%% @private
generate_folds_for_class(_, [], _, Current, Acc ) ->
    {Acc, Current};
generate_folds_for_class(Class, [Id|ExIds], Folds, CurrentFold, Acc) ->
    Current = if Folds < CurrentFold -> 1; true -> CurrentFold end,
    generate_folds_for_class(Class, ExIds, Folds, Current + 1,
			     dict:update(Current, fun ([{C, N, Ids}]) ->
							  [{C, N+1, [Id|Ids]}]
						  end, Acc)).
init_folds(Folds, Init, Test) ->
    Init0 = dict:fetch(Init, Folds),
    TestSet0 = dict:fetch(Test, Folds),
    Rest0 = dict:erase(Init, Folds),
    {Init0, TestSet0, dict:erase(Test, Rest0)}.

%% @doc merge all folds in Folds expect for test
-spec merge_folds(dict(), integer()) -> {Test::examples(), Train::examples()}.
merge_folds(Folds, Test) ->
    {Init, TestSet, Rest} = case Test of
				1 -> 
				    init_folds(Folds, 2, Test);
				_Other ->
				    init_folds(Folds, 1, Test)
			    end,
    Train = dict:fold(fun (_, Fold, Acc) ->
			      lists:zipwith(fun ({Class, Ca, Ia}, {Class, Cb, Ib}) ->
						    {Class, Ca+Cb, Ia++Ib}
					    end, Fold, Acc)
		      end, Init, Rest),
    {TestSet, Train}.

%% @doc stratified cross validation
-spec cross_validation(fun((Train::examples(), Test::examples(), Fold::integer()) -> []), integer(), examples()) -> [].
cross_validation(Fun, NoFolds, Examples) ->
    Folds = generate_folds(Examples, NoFolds),
    cross_validation(Fun, Folds, NoFolds, NoFolds, []).

cross_validation(_Fun, _Folds, _NoFolds, 0, Acc) -> 
    lists:reverse(Acc);
cross_validation(Fun, Folds, NoFolds, CurrentFold, Acc) -> 
    {Test, Train} = merge_folds(Folds, CurrentFold),
    Result = Fun(Train, Test, NoFolds - CurrentFold + 1),
    cross_validation(Fun, Folds, NoFolds, CurrentFold - 1, [Result|Acc]). 

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
	    NewInBag = duplicate_example(ExId, Times, InBag),
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

-ifdef(TEST).

mock_examples(Mock) ->
    lists:foldl(fun ({Class, Count}, Acc) ->
			[{Class, Count, []}|Acc]
		end, [], Mock).

mock_split(Left, Right) ->
    {both, mock_examples(Left), mock_examples(Right)}.
-endif.
