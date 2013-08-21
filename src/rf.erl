%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rf).

-define(DATE, "2013-05-16").
-define(MAJOR_VERSION, "1").
-define(MINOR_VERSION, "0").
-define(REVISION, "0.1").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").
-export([
	 main/1,
	 
	 help/0, 
	 new/1,  
	 build/4,
	 partial_build/1,

	 evaluate/4,
	 partial_evaluate/1,

	 predict/4,
	 save/3,
	 load/1,

	 serialize/2,
	 unserialize/1,
	 
	 args/2,

	 kill/1, 
	 killer/1
	]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CMD_SPEC,
	[{<<"help">>,           $h,           "help",         undefined,
	  "Show this usage information."},
	 {<<"version">>,        undefined,    "version",      undefined,
	  "Show the program version."},
	 {<<"examples">>,       undefined,    "examples",     undefined,
	  "View example usages"},
	 {<<"observer">>,        undefined,    "observer",     undefined,
	  "Observe the execution (cpu/memory etc.)"},
	 {<<"input">>,          $i,           "input",        string, 
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."},
	 {<<"cores">>,          $c,           "cores",        {integer, erlang:system_info(schedulers)},
	  "Number of cores used by the algorightm for constructing the model."},

	 {<<"mode">>,           $x,           "mode",        atom,
	  "Mode for building and/or evaluation. Available options include: 'cv', 'split', 'build' and 'evaluate'."},

	 {<<"ratio">>,          $r,           "ratio",       {float, 0.66},
	  "Splitting ratio (i.e. the fraction of training examples). This argument is only valid when 'split' is activated."},
	 {<<"folds">>,          undefined,    "folds",       {integer, 10},
	  "Number of cross validation folds"},
	 {<<"model_file">>,     undefined,    "model-file",  string,
	  "File name when writing a model to file (only applicable when using the 'build' or 'evaluate'-argument')."},

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

	 {<<"weight_factor">>,  undefined,    "weight-factor", {float, 0.5},
	  "Used for controlling the randomness of the 'combination' and 'weighted'-arguments."},
	 {<<"no_resamples">>,   undefined,    "no-resample", {integer, 6},
	  "Number of re-samples."},
	 {<<"min_gain">>,       undefined,    "min-gain",    {float, 0},
	  "Minimum allowed gain for not re-sampling (if the 'resample'-argument is specified)."},
	 
	 {<<"no_features">>,    undefined,    "no-features", {string, <<"default">>},
	  "Number of features to inspect at each split. If set to log log(F)+1, where F denotes the total number of features, are inspected. The default value is usually a good compromise between diversity and performance."},
	 {<<"no_rules">>,       undefined,    "no-rules",    {string, <<"ss">>},
	  "Number of rules to generate (from n features, determined by 'no-features'). Options include: 'default', then 'no-features' div 2, 'same', then 'no-features' is used otherwise n is used."},

	 {<<"output_predictions">>, $y,       "output-predictions", {boolean, false},
	  "Write the predictions to standard out."},
	 {<<"variable_importance">>, $v, "variable-importance",     {integer, 0},
	  "Output the n most important variables calculated using the reduction in information averaged over all trees for each feature."},
	 {<<"output">>,         $o,           "output",      {atom, default},
	  "Output format. Available options include: 'default' and 'csv'. If 'csv' is selected output is formated as a csv-file (see Example 5)"}
	]).

%% @doc show help using rr:show_help()
help() ->
    rr:show_help(options, ?CMD_SPEC, "rf").

%% @doc suspend a random forest model
kill(Model) ->
    rr_ensemble:kill(Model).

%% @doc build a model
build(Rf, Features, Examples, ExConf) ->
    rr_ensemble:generate_model(Features, Examples, ExConf, Rf).

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

    Accuracy = rr_eval:accuracy(Dict),
    Auc = rr_eval:auc(ClassesInTest, Dict, NoTestExamples),
    AvgAuc = lists:foldl(fun
			     ({_, {_, 'n/a'}}, Sum) -> 
				 Sum;
			     ({_, {No, A}}, Sum) -> 
				 Sum + No/NoTestExamples*A
			 end, 0, Auc),
    Precision = rr_eval:precision(ClassesInTest, Matrix),
    Recall = rr_eval:recall(ClassesInTest, Matrix),
    Brier = rr_eval:brier(Dict, NoTestExamples),
    [{accuracy, Accuracy},
     {auc, {Auc, AvgAuc}}, 
     {strength, Strength},
     {correlation, Correlation},
     {variance, Variance},
     {c_s2, Correlation/math:pow(Strength, 2)},
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
    rr_system:serialize_model(Dump).

%% @doc unserialize a model into a "thing" fit for loading with rr_ensemble:load_model/1
unserialize(Dump) ->
    rr_system:unserialize_model(Dump).

save(File, Rf, Model) ->
    Data = serialize(Rf, Model),
    file:write_file(File, Data).

load(File) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    Model = unserialize(Binary),
	    rr_ensemble:load_model(Model);
	{error, Reason} ->
	    {error, Reason}
    end.		

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
				  fun rf_missing:weighted/5),
    Progress = proplists:get_value(progress, Props, fun (_, _) -> ok end),
    Score = proplists:get_value(score, Props, rf_tree:info()),
    NoTrees = proplists:get_value(no_trees, Props, 100),
    Prune = proplists:get_value(pre_prune, Props, 
				rf_tree:example_depth_stop(2, 1000)),

    FeatureSampling = proplists:get_value(feature_sampling, Props,
					  rf_branch:subset()),
    ExampleSampling = proplists:get_value(example_sampling, Props,
					  fun rr_example:bootstrap_aggregate/1),
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

%% @todo refactor to use proplist
main(Args) ->
    Options = rr:parse(Args, ?CMD_SPEC),
    case rr:any_opt([<<"help">>, <<"version">>, 
		     <<"examples">>, <<"observer">>], Options) of
	<<"help">> ->
	    help(),
	    halt();
	<<"version">> ->
	    io:format(show_information()),
	    halt();
	<<"examples">> ->
	    io:format(show_examples()),
	    halt();
	<<"observer">> ->
	    observer:start();
	false ->
	    ok
    end,
    InputFile = case proplists:get_value(<<"input">>, Options) of
		    undefined ->
			rr:illegal("no input file defined"),
			halt();
		    File0 -> File0
		end,
    Cores = proplists:get_value(<<"cores">>, Options),
    Output = output(Options),

    rr_log:info("loading '~s' on ~p core(s)", [InputFile, Cores]),
    LoadingTime = now(),
    Csv = csv:binary_reader(InputFile),
    ExSet = rr_example:load(Csv, Cores),
    rr_log:debug("loading took '~p' second(s)", [rr:seconds(LoadingTime)]),

    RfArgs = args(Options, fun rr:illegal_option/2),
    Progress = args(<<"progress">>, Options, fun rr:illegal_option/2),
    Rf = rf:new([{base_learner, rf_tree}, {progress, Progress}|RfArgs]),
    
    Build = partial_build(Rf),
    Evaluate = partial_evaluate(Rf),
    
    ExperimentTime = now(),
    case proplists:get_value(<<"mode">>, Options) of
	split ->
	    Ratio = proplists:get_value(<<"ratio">>, Options),
	    {Res, _Models} = 
		split_validation:evaluate(ExSet,[{build, Build}, 
						 {evaluate, killer(Evaluate)}, 
						 {ratio, Ratio}]),
	    Output(Res);
	cv ->
	    CvProgress = fun (Fold) -> 
				 io:format(standard_error, "fold ~p ", [Fold])
			 end,
	    Folds = proplists:get_value(<<"folds">>, Options),
	    {Res, _Models} = 
		cross_validation:evaluate(ExSet, [{build, Build}, 
						  {evaluate, killer(Evaluate)}, 
						  {progress, CvProgress},
						  {folds, Folds}]),
	    Output(Res);
	build ->
	    Model = Build(ExSet#rr_exset.features,
			  ExSet#rr_exset.examples,
			  ExSet#rr_exset.exconf),
	    File = proplists:get_value(<<"model_file">>, Options, "undefined-model.rr"),
	    save(File, Rf, Model);
	evaluate ->
	    File = proplists:get_value(<<"model_file">>, Options),
	    Cores = proplists:get_value(<<"cores">>, Options, 4),
	    {Model, Conf} = load(File),
	    Res = evaluate(Conf, Model, 
			   ExSet#rr_exset.examples, 
			   ExSet#rr_exset.exconf),
	    Output(Res);
	Other ->
	    rr:illegal_option("mode", Other)
    end,
    rr_log:info("experiment took '~p' second(s)", [rr:seconds(ExperimentTime)]),
    case proplists:get_value(<<"observer">>, Options) of
	true -> rr_log:info("press ^c to exit"), receive wait -> wait end;
	undefined -> ok
    end,
    csv:kill(Csv),
    rr_example:kill(ExSet),
    rr_config:stop(),
    rr_log:stop().

%% @doc kill (to clean up unused models) after evaluation (to reduce
%% memory footprint during cross validation)
killer(Evaluate) ->
    fun (Model, Test, ExConf) ->
	    Result = Evaluate(Model, Test, ExConf),
	    kill(Model),
	    Result
    end.

%% @doc convert key from arguments to a function for the rf agorithm
%% Proplist must contain: {no_features, NoFeatures}
%% @end
args(Key, Rest, Error) ->
    Value = proplists:get_value(Key, Rest),
    case Key of
	<<"example_sampling">> ->
	    example_sampling(Value, Error);
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
	O when O == <<"cores">>;
	       O == <<"min_examples">>;
	       O == <<"max_depth">>;
	       O == <<"no_trees">> ->
	    Value;
	_ ->
	    undefined
    end.

%% @doc get all important args
args(Rest, Prior) ->
    NoFeatures = args(<<"no_features">>, Rest, Prior),
    MinEx = args(<<"min_examples">>, Rest, Prior),
    MaxDepth = args(<<"max_depth">>, Rest, Prior),
    Args = [{no_features, NoFeatures},
	    {no_cores, args(<<"cores">>, Rest, Prior)},
	    {no_trees, args(<<"no_trees">>, Rest, Prior)},
	    {score, args(<<"score">>, Rest, Prior)},
	    {missing_values, args(<<"missing">>, Rest, Prior)},
	    {pre_prune, rf_tree:example_depth_stop(MinEx, MaxDepth)},
	    {feature_sampling, args(<<"feature_sampling">>, Rest, Prior)},
	    {example_sampling, args(<<"example_sampling">>, Rest, Prior)},
	    {distribute, args(<<"distribute">>, Rest, Prior)},
	    {base_learner, rf_tree}],
    lists:filter(fun ({_Key, Value}) -> Value =/= undefined end, Args).

example_sampling(Value, Error) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"subagging">> ->
	    fun rr_example:subset_aggregate/1;
	<<"bagging">> ->
	    fun rr_example:bootstrap_aggregate/1;
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
	<<"random">> -> fun rf_missing:random/5;
	<<"randomw">> -> fun rf_missing:random_weighted/5;
	<<"weighted">> -> fun rf_missing:weighted/5;
	<<"partition">> -> fun rf_missing:random_partition/5;
	<<"wpartition">> -> fun rf_missing:weighted_partition/5;
	<<"proximity">> -> fun rf_missing:proximity/5;
	<<"right">> -> fun rf_missing:right/5;
	<<"left">> -> fun rf_missing:left/5;
	<<"ignore">> -> fun rf_missing:ignore/5;
	Other -> Error("missing", Other)
    end.

output(Options) ->
    case proplists:get_value(<<"output">>, Options) of
	default -> rr_result:default();
	csv -> rr_result:csv();
	Other -> rr:illegal_option("output", Other)
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
	Other -> Error("score", Other)		
    end.

no_features(Value, Error) ->
    case Value of
	<<"default">> -> fun (TotalNoFeatures) -> trunc(math:log(TotalNoFeatures)/math:log(2)) + 1 end;
	<<"sqrt">> -> fun (TotalNoFeatures) -> trunc(math:sqrt(TotalNoFeatures)) end;
	X ->
	    case rr_example:format_number(X) of
		{true, Number} when Number > 0 -> fun (_) -> Number end;
		_ -> Error("no-features", X)
	    end
    end.

feature_sampling(Value, Error, Options) ->
    case rr_util:safe_iolist_to_binary(Value) of
	<<"weka">> ->
	    rf_branch:weka();
	<<"resample">> ->
	    NoResamples = proplists:get_value(<<"no_resamples">>, Options, 6),
	    MinGain = proplists:get_value(<<"min_gain">>, Options),
	    rf_branch:resample(NoResamples, MinGain);
	<<"combination">> ->
	    Factor = proplists:get_value(weight_factor, Options),
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
	    Weight = proplists:get_value(weight_factor, Options),
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
	%% <<"depth-rule">> ->
	%%     NoRules = args(<<"no_rules">>, Options, Error),
	%%     RuleScore = args(<<"rule_score">>, Options, Error),
	%%     rf_branch:depth_rule(NoRules, RuleScore);
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

show_examples() ->
    "Example 1: 10-fold cross validation 'car' dataset:
  > ./rr -i data/car.txt -x --folds 10 > result.txt

Example 2: 0.66 percent training examples, 'heart' dataset. Missing
values are handled by weighting a random selection towards the most
dominant branch.
  > ./rr -i data/heart.txt -s -r 0.66 --missing weighted > result.txt

Example 3: 10-fold cross validation on a sparse dataset using re-sampled
feature selection
  > ./rr -i data/sparse.txt -x --resample > result.txt

Example 4: 0.7 percent training examples, 'heart' dataset. Missing
values are handled by by weighting examples with missing values
towards the most dominant branch. Each node in the tree is composed
of a rule (1..n conjunctions) and the 20 most importante variables
are listed.
  > ./rr -i data/heart.txt -s --ratio 0.7 --rule -v 20 > result.txt

Example 5: 0.7 percent training examples, for multiple dataset and output
evaluations csv-formated. This could be achieved using a bash-script:
  > for f in data/*.txt; do ./rr -i \"$f\" -s -r 0.7 -o csv >> result.csv; 
done~n".

show_information() -> 
    io_lib:format("rf (Random Forest Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s

Written by ~s ~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, 
		    ?DATE, ?AUTHOR, ?AUTHOR]).

-ifdef(TEST).
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
      {timeout, 60, profile("../data/spambase.txt", "PROFILE_SPAMBASE.txt")}}
    ].

profile(In, Out) ->
    fun() ->
	    File = csv:binary_reader(In),
	    {Features, Examples, Dataset} = rr_example:load(File, 4),
	    NoFeatures = fun (NoFeatures) -> trunc(math:log(NoFeatures)/math:log(2)) end,
	    {Build, _Evaluate, _} = rf:new([{no_features, NoFeatures}]),
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
