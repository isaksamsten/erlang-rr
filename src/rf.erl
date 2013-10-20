%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rf).
-export([
	 %% rr_command behaviour
	 parse_args/1,	 
	 help/0, 
	 args/2,
	 args/1,

	 %% rr_classifier behaviour
	 new/1,  
	 build/2,
	 evaluate/4,
	 partial_build/1,
	 partial_evaluate/1,
	 serialize/2,
	 unserialize/1,

	 %% other
	 build/4,
	 
	 variable_importance/2,
	 predict/4,
	 get/1,
	 kill/1
	]).

-behaviour(rr_command).
-behaviour(rr_classifier).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CMD_SPEC,
	[{<<"cores">>,          $c,           "cores",        {integer, erlang:system_info(schedulers)},
	  "Number of cores used by the algorightm for constructing the model."},
	 {<<"progress">>,       undefined,    "progress",    {string, <<"dots">>},
	  "Show a progress bar while building a model. Available options include: 'dots', 'numeric' and 'none'. "},

	 {<<"score">>,          undefined,    "score",       {string, <<"info">>},
	  "Defines the measure, which should be minimized, for evaluating the goodness of split points in each branch. Available options include: 'info', 'gini' and 'gini-info', where 'info' denotes information entropy and 'gini' the gini-impurity."},

	 {<<"rule_score">>,     undefined,    "rule-score",  {string, <<"laplace">>},
	  "Defines the measure, which should be minimized, for evaluating the goodness of a specific rule. Available otpions include: 'm', 'laplace' and 'purity', where 'm' denotes the m-estimate"},
	 {<<"no_trees">>,    $n,           "no-trees",    {integer, 10},
	  "Defines the number of classifiers (trees) to build."},

	 {<<"max_depth">>,      undefined,    "max-depth",   {integer, 1000},
	  "Defines the maximum allowed depth for a single decision tree."},
	 {<<"min_examples">>,    undefined,    "min-examples",{integer, 1},
	  "Min number of examples allowed for splitting a node"},
	 
	 {<<"feature_sampling">>, undefined,    "feature-sampling", {string, <<"subset">>},
	  "Select a method for feature sampling. Available options include: 'subset', 'rule', 'random-rule', 'resample', 'weka', and 'combination'."},
	 
	 {<<"missing">>,        $m,           "missing",     {string, <<"weighted">>},
	  "Distributing missing values according to different strategies. Available options include: 'random', 'randomw', 'partitionw', 'partition', 'weighted', 'left', 'right' and 'ignore'. If 'random' is used, each example with missing values have an equal probability of be distributed over the left and right branch. If 'randomw' is selected, examples are randomly distributed over the left and right branch, but weighted towards the majority branch. If 'partition' is selected, each example is distributed equally over each branch. If 'partitionw' is selected, the example are distributed over each branch but weighted towards the majority branch. If 'weighted' is selected, each example is distributed over the majority branch. If 'left', 'right' or 'ignore' is selected, examples are distributed either to the left, right or is ignored, respectively."},

	 {<<"distribute">>,     $d,           "distribute",  {string, <<"default">>},
	  "Distribute examples at each split according to different strategies. Available option include: 'default' or 'rulew'. If 'default' is selected, examples are distributed to either left or right. If 'rulew' is selected, fractions of each example are distributed according to how many antecedents each rule-node classifies the example."},

	 {<<"example_sampling">>, undefined,  "example-sampling", {string, <<"bagging">>},
	  "Select the method for feature sampling. Available options include: 'bagging' and 'subagging'."},
	 {<<"variance_options">>, undefined, "variance-options", {string, "0.3,1,100,10"},
	  "Options for the triangle variance sampling method. Format: 'Threshold A B C'"},

	 {<<"weight_factor">>,  undefined,    "weight-factor", {float, 0.5},
	  "Used for controlling the randomness of the 'combination' and 'weighted'-arguments."},
	 {<<"no_resamples">>,   undefined,    "no-resample", {integer, 6},
	  "Number of re-samples."},
	 {<<"min_gain">>,       undefined,    "min-gain",    {float, 0},
	  "Minimum allowed gain for not re-sampling (if the 'resample'-argument is specified)."},
	 
	 {<<"no_features">>,    undefined,    "no-features", {string, <<"default">>},
	  "Number of features to inspect at each split. If set to log log(F)+1, where F denotes the total number of features, are inspected. The default value is usually a good compromise between diversity and performance."},
	 {<<"no_rules">>,       undefined,    "no-rules",    {string, <<"ss">>},
	  "Number of rules to generate (from n features, determined by 'no-features'). Options include: 'default', then 'no-features' div 2, 'same', then 'no-features' is used otherwise n is used."}

%	 {<<"output_predictions">>, $y,       "output-predictions", {boolean, false},
%	  "Write the predictions to standard out."},
%	 {<<"variable_importance">>, $v, "variable-importance",     {integer, 0},
%	  "Output the n most important variables calculated using the reduction in information averaged over all trees for each feature."},

	]).
-define(NAME, "rf").

%% @doc show help using rr:show_help()
help() ->
    rr:show_help(options, ?CMD_SPEC, "rf").

%% @doc suspend a random forest model
kill(Model) ->
    rr_ensemble:kill(Model).

%% @doc get the underlying model
get(Model) ->
    rr_ensemble:get_base_classifiers(Model).

%% @doc build a model
%% @deprecate
build(Rf, Features, Examples, ExConf) ->
    rr_ensemble:generate_model(Features, Examples, ExConf, Rf).

%% @doc
variable_importance(Model, Rf) ->
    rr_ensemble:variable_importance(Model, Rf).

build(Rf, ExSet) ->
    build(Rf, ExSet#rr_exset.features, ExSet#rr_exset.examples, ExSet#rr_exset.exconf).

%% @doc predict the class label of Example using Model and Rf
predict(Rf, Model, Example, ExConf) ->
    rr_ensemble:predict_majority(Model, Example, ExConf, Rf).

%% @private evaluate Model using som well knonw evaluation metrics
evaluate(Conf, Model, Test, ExConf) ->
    NoTestExamples = rr_example:count(Test),
    ClassesInTest = lists:map(fun ({Class, _, _}) -> Class end, Test),

    Dict = rr_ensemble:evaluate_model(Model, Test, ExConf, Conf),
    Matrix = rr_eval:confusion_matrix(Dict),

    OOBAccuracy = rr_ensemble:oob_accuracy(Model, Conf),
    {BaseAccuracy, Corr} = rr_ensemble:base_accuracy(Model, Test, ExConf, Conf),
    
    Strength = rr_eval:strength(Dict, NoTestExamples),
    Variance = rr_eval:variance(Dict, NoTestExamples),
    Correlation = rr_eval:correlation(Dict, NoTestExamples, Corr, 
				      Conf#rr_ensemble.no_classifiers),
    NoRules = rr_ensemble:no_rules(Model, Conf),
    Accuracy = rr_eval:accuracy(Dict),
    Auc = rr_eval:auc(ClassesInTest, Dict, NoTestExamples),
 
    Precision = rr_eval:precision(ClassesInTest, Matrix),
    Recall = rr_eval:recall(ClassesInTest, Matrix),
    Brier = rr_eval:brier(Dict, NoTestExamples),
    [{accuracy, Accuracy},
     {auc, Auc}, 
     {no_rules, NoRules},
     {strength, Strength},
     {correlation, Correlation},
     {variance, Variance},
     {c_s2, if Strength =/= 0.0 -> Correlation/math:pow(Strength, 2); true -> 0.0 end},
     {precision, Precision},
     {recall, Recall},
     {oob_base_accuracy, OOBAccuracy},
     {base_accuracy, BaseAccuracy},
     {brier, Brier}].

%% @doc 
%% serialize configuration Rf togheter with Model 
%% (i.e. remember both the model and the conf that built it) 
%% @end
serialize(Rf, Model) ->
    Dump = rr_ensemble:get_model(Model, Rf),
    rr_system:serialize_model(?MODULE, Dump).

%% @doc unserialize a model into a Model
unserialize(Model) ->
    rr_ensemble:load_model(Model).

%% @doc return a build fun
partial_build(Rf) ->
    fun (Features, Examples, ExConf) ->
	    build(Rf, Features, Examples, ExConf)
    end.

%% @doc return an evaluate fun
partial_evaluate(Rf) ->
    fun (Model, Examples, ExConf) ->
	    evaluate(Rf, Model, Examples, ExConf)
    end.

%% @doc create a new rf-model
new(Props) ->
    NoFeatures = proplists:get_value(
		   no_features, Props,
		   fun (T) -> 
			   trunc(math:log(T)/math:log(2)) + 1 
		   end),
    Cores = proplists:get_value(no_cores, Props, 
				erlang:system_info(schedulers)),
    Missing = proplists:get_value(missing_values, Props, 
				  fun rf_missing:weighted/6),
    Progress = proplists:get_value(progress, Props, fun (_, _) -> ok end),
    Score = proplists:get_value(score, Props, rf_tree:info()),
    NoTrees = proplists:get_value(no_trees, Props, 100),
    Prune = proplists:get_value(pre_prune, Props, 
				rf_tree:example_depth_stop(2, 1000)),

    FeatureSampling = proplists:get_value(feature_sampling, Props,
					  rf_branch:subset()),
    ExampleSampling = proplists:get_value(example_sampling, Props,
					  fun rr_sampling:bootstrap_replicate/1),
    Distribute = proplists:get_value(distribute, Props,
				     fun rr_example:distribute/3),
    BaseLearner = proplists:get_value(base_learner, Props, rf_tree),
    Split = proplists:get_value(split, Props, fun rf_tree:random_split/5),
    Tree = #rf_tree{
	      score = Score,
	      prune = Prune,
	      branch = FeatureSampling,
	      split = Split,
	      distribute = Distribute,
	      missing_values = Missing,
	      no_features = NoFeatures
	     },
    #rr_ensemble {
       progress = Progress,
       bagging = ExampleSampling,
       no_classifiers = NoTrees,
       base_learner = {BaseLearner, Tree},
       cores = Cores
      }.

%% @doc parse args for this classifier
parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

%% @doc default args function throws error if argument is not found
args(Args) ->
    args(Args, fun (Value, Reason) -> throw({bad_arg, "rf", Value, Reason}) end).
		       
%% @doc get all important args
args(Rest, Error) ->
    NoFeatures = args(<<"no_features">>, Rest, Error),
    MinEx = args(<<"min_examples">>, Rest, Error),
    MaxDepth = args(<<"max_depth">>, Rest, Error),
    Progress = args(<<"progress">>, Rest, Error),
    Args = [{no_features, NoFeatures},
	    {progress, Progress},
	    {no_cores, args(<<"cores">>, Rest, Error)},
	    {no_trees, args(<<"no_trees">>, Rest, Error)},
	    {score, args(<<"score">>, Rest, Error)},
	    {missing_values, args(<<"missing">>, Rest, Error)},
	    {pre_prune, rf_tree:example_depth_stop(MinEx, MaxDepth)},
	    {feature_sampling, args(<<"feature_sampling">>, Rest, Error)},
	    {example_sampling, args(<<"example_sampling">>, Rest, Error)},
	    {distribute, args(<<"distribute">>, Rest, Error)},
	    {base_learner, rf_tree}],
    lists:filter(fun ({_Key, Value}) -> Value =/= undefined end, Args).

%% @doc convert key from arguments to a function for the rf agorithm
args(Key, Rest, Error) ->
    Value = proplists:get_value(Key, Rest),
    case Key of
	<<"example_sampling">> ->
	    example_sampling(Value, Error, Rest);
	<<"feature_sampling">> ->
	    feature_sampling(Value, Error, Rest);
	<<"score">> ->
	    score(Value, Error, Rest);
	<<"progress">> ->
	    progress(Value, Error);
	<<"no_features">> ->
	    no_features(Value, Error);
	<<"missing">> ->
	    missing_values(Value, Error);
	<<"distribute">> ->
	    distribute(Value, Error);
	<<"no_rules">> ->
	    no_rules(Value, Error); %todo: refactor prior
	<<"rule_score">> ->
	    rule_score(Value, Error);
	_ ->
	    Value
    end.

example_sampling(Value, Error, Options) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"subagging">> ->
	    fun rr_example:subset_aggregate/1;
	<<"bagging">> ->
	    fun rr_sampling:bootstrap_replicate/1;
	<<"triangle-variance">> ->
	    Option = args(<<"variance_options">>, Options, Error),
	    [Threshold, A, B, C] = rr:parse_option_string(Option),
	    rr_sampling:triangle_variance_sample(Threshold, A, B, C);
	<<"uniform-variance">> ->
	    Option = args(<<"variance_options">>, Options, Error),
	    [Threshold, Min, Max] = rr:parse__option_string(Option),
	    rr_sampling:uniform_variance_sample(Threshold, Min, Max);
	<<"random">> ->
	    fun rr_sampling:random/1;
	<<"undersample">> ->
	    fun rr_sampling:undersample_replicate/1;
	<<"nothing">> ->
	    fun (Examples) -> {Examples, []} end;
	Other ->
	    Error("example-sampling", Other)		
    end.

distribute(Value, Error) ->	
    case rr_util:safe_iolist_to_binary(Value) of
	<<"default">> -> fun rr_example:distribute/3;
	<<"rulew">> -> fun rr_rule:distribute_weighted/3;
	Other -> Error("distribute", Other)
    end.

missing_values(Value, Error) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"random">> -> fun rf_missing:random/6;
	<<"randomw">> -> fun rf_missing:random_weighted/6;
	<<"weighted">> -> fun rf_missing:weighted/6;
	<<"partition">> -> fun rf_missing:random_partition/6;
	<<"wpartition">> -> fun rf_missing:weighted_partition/6;
	<<"proximity">> -> fun rf_missing:proximity/6;
	<<"right">> -> fun rf_missing:right/6;
	<<"left">> -> fun rf_missing:left/6;
	<<"ignore">> -> fun rf_missing:ignore/6;
	Other -> Error("missing", Other)
    end.

progress(Value, Error) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"dots">> -> fun
			  (done, done) -> io:format(standard_error, "~n", []);
			  (_, _) -> io:format(standard_error, "..", [])
		      end;
	<<"numeric">> -> fun
			     (done, done) -> 
				 io:format(standard_error, "~n", []);
			     (Id, T) -> 
				 io:format(standard_error, "~p/~p.. ", [Id, T])
			 end;
	<<"none">> -> fun(_, _) -> ok end;
	Other -> Error("progress", Other)
    end.

score(Value, Error, Options) ->
    WeightFactor = proplists:get_value(weight_factor, Options, 0.5),
    case rr_util:safe_iolist_to_binary(Value) of
	<<"info">> -> rf_tree:info();
	<<"gini">> -> rf_tree:gini();
	<<"gini-info">> -> rf_tree:gini_info(WeightFactor);
	<<"hellinger">> -> fun rr_estimator:hellinger/2;
	<<"squared-chord">> -> fun rr_estimator:squared_chord/2;
	<<"jensen-difference">> -> fun rr_estimator:jensen_difference/2;
	<<"bhattacharyya">> -> fun rr_estimator:bhattacharyya/2;
	Other -> Error("score", Other)		
    end.

no_features(Value, Error) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"default">> -> fun (TotalNoFeatures) -> trunc(math:log(TotalNoFeatures)/math:log(2)) + 1 end;
	<<"sqrt">> -> fun (TotalNoFeatures) -> trunc(math:sqrt(TotalNoFeatures)) end;
	<<"all">> -> fun (TotalNoFeatures) -> TotalNoFeatures end;
	X ->
	    io:format("~p~n",[X]),
	    case rr_example:format_number(X) of
		{true, Number} when Number > 0 -> fun (_) -> Number end;
		_ -> Error("no-features", X)
	    end
    end.

feature_sampling(Value, Error, Options) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"infinite-resample">> ->
	    rf_branch:weka();
	<<"hellinger">> ->
	    rf_branch:hell();
	<<"resample">> ->
	    NoResamples = proplists:get_value(<<"no_resamples">>, Options, 6),
	    MinGain = proplists:get_value(<<"min_gain">>, Options),
	    rf_branch:resample(NoResamples, MinGain);
	<<"combination">> ->
	    Factor = proplists:get_value(<<"weight_factor">>, Options),
	    rf_branch:random_correlation(Factor);
	<<"rule">> ->
	    NoRules = args(<<"no_rules">>, Options, Error),
	    RuleScore = args(<<"rule_score">>, Options, Error),
	    rf_branch:rule(NoRules, RuleScore); 
	<<"random-rule">> ->
	    Factor = proplists:get_value(weight_factor, Options),
	    NoRules = args(<<"no_rules">>, Options, Error),
	    RuleScore = args(<<"rule_score">>, Options, Error),
	    rf_branch:random_rule(NoRules, RuleScore, Factor);
	<<"choose-rule">> ->
	    NoRules = args(<<"no_rules">>, Options, Error),
	    RuleScore = args(<<"rule_score">>, Options, Error),
	    rf_branch:choose_rule(NoRules, RuleScore);
	<<"subset">> -> 
	    rf_branch:subset();
	<<"random-chisquare">> ->
	    Weight = proplists:get_value(<<"weight_factor">>, Options),
	    rf_branch:random_chisquare(Weight);
	<<"chisquare">> ->
	    F = proplists:get_value(<<"weight_factor">>, Options),
	    rf_branch:chisquare(F);
	<<"resquare">> ->
	    F = proplists:get_value(<<"weight_factor">>, Options),
	    rf_branch:randomly_resquare(0.5, F);
	<<"chisquare-decrease">> ->
	    F =  proplists:get_value(<<"weight_factor">>, Options),
	    rf_branch:chisquare_decrease(0.5, F);
	<<"random-subset">> ->
	    rf_branch:random_subset(0);
	<<"sample-examples">> ->
	    Factor = proplists:get_value(weight_factor, Options),
	    rf_branch:sample_examples(0.1, Factor);
	Other ->
	    Error("feature-sampling", Other)
    end.

%% @todo rename and fix
no_rules(Value, Error) ->
    NoFeatures = 10,
    case rr_util:safe_iolist_to_binary(Value) of
	<<"sh">> -> {NoFeatures, NoFeatures div 2};
	<<"ss">> -> fun (Total) -> trunc(math:sqrt(Total)) + 1 end;
	<<"sd">> -> {NoFeatures, NoFeatures * 2};
	<<"ds">> -> {NoFeatures * 2, NoFeatures};
	<<"dd">> -> {NoFeatures * 2, NoFeatures * 2};
	<<"dh">> -> {NoFeatures * 2, NoFeatures div 2};
	<<"hh">> -> {NoFeatures div 2, NoFeatures div 2};
	<<"hs">> -> {NoFeatures div 2, NoFeatures};
	<<"hd">> -> {NoFeatures div 2, NoFeatures * 2};	
	Other -> Error("no-rules", Other)
    end.
    
rule_score(Value, Error) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"laplace">> -> fun rf_rule:laplace/2;
	<<"m">> -> fun rf_rule:m_estimate/2;
	<<"purity">> -> fun rf_rule:purity/2;
	Other -> Error("rule-score", Other)					  
    end.

-ifdef(TEST).
variable_importance_test() ->
    random:seed({10, 10, 10}),
    File = csv:binary_reader("../data/iris.txt"),
    ExSet = rr_example:load(File, 4),
    NoFeatures = fun (NoFeatures) -> trunc(math:log(NoFeatures)/math:log(2)) end,
    Rf = rf:new([{no_features, NoFeatures}, {no_trees, 500}, {score, fun rr_estimator:info_gain/2}]),
    Model = rf:build(Rf, ExSet),
    Vi = rf:variable_importance(Model, Rf),
    {MaxFeature, MaxScore} = rr_util:max(fun ({K, V}) -> V end, dict:to_list(Vi)),
    ?assertEqual(true,  MaxFeature == 3 orelse MaxFeature == 4).

-ifdef(PROFILE).
profile_test_() ->
    profile_tests().

profile_tests() -> 
    [
     {"Profile car dataset", 
      {timeout, 60, profile("../data/car.txt", "PROFILE_CAR.txt")}},
     {"Profile iris dataset", 
      {timeout, 60, profile("../data/iris.txt", "PROFILE_IRIS.txt")}},
     {"Profile spambase dataset", 
      {timeout, 60, profile("../data/spambase.txt", "PROFILE_SPAMBASE.txt")}},
     {"Profile spambase dataset hellinger", 
      {timeout, 60, profile("../data/spambase.txt", "PROFILE_SPAMBASE_HELL.txt", 
			    [{score, fun rr_estimator:hellinger/2}])}}
    ].
profile(In, Out) ->
    profile(In, Out, []).

profile(In, Out, Props) ->
    fun() ->
	    File = csv:binary_reader(In),
	    #rr_exset {
	       features=Features, 
	       examples=Examples, 
	       exconf=Dataset
	      } = rr_example:load(File, 4),
	    NoFeatures = fun (NoFeatures) -> trunc(math:log(NoFeatures)/math:log(2)) end,
	    Rf = rf:new([{no_features, NoFeatures}] ++ Props),
	    Build = rf:partial_build(Rf),
	    eprof:start(),
	    eprof:log(Out),
	    eprof:profile(
	      fun() ->
		      Build(Features, Examples, Dataset)
	      end),
	    eprof:analyze(total),
	    eprof:stop()
    end.

-endif.
-endif.
