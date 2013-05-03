%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').

-define(DATE, "2013-04-15").
-define(MAJOR_VERSION, "0").
-define(MINOR_VERSION, "4").
-define(REVISION, "1.0").

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").

%% @headerfile "rr_tree.hrl"
-include("rr_tree.hrl").

-define(CMD_SPEC,
	[{help,           $h,           "help",         undefined,
	  "Show this usage information."},
	 {version,        undefined,    "version",      undefined,
	  "Show the program version."},
	 {examples,       undefined,    "examples",     undefined,
	  "View example usages"},
	 {input_file,     $i,           "input",        string, 
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."},
	 {cores,          $c,           "cores",        {integer, erlang:system_info(schedulers)},
	  "Number of cores used by the algorightm for constructing the model."},
	 
	 {split,          $s,           "split",       undefined,
	  "Evaluate the input dataset by splitting it into two disjoint subsets determined by the 'ratio'-argument. The ratio determines the number of training examples."},
	 {ratio,          $r,           "ratio",       {float, 0.66},
	  "Splitting ratio (i.e. the fraction of training examples). This argument is only valid when 'split' is activated."},
	 {cv,             $x,           "cross-validate", undefined,
	  "Split the dataset into k disjoint subsets (determined by the 'folds'-argument) and build and evaluate a model by combining each k-1 folds and train on one fold repeatedly k times."},
	 {folds,          undefined,    "folds",       {integer, 10},
	  "Number of cross validation folds"},
	 {build,          $b,           "build",       undefined,
	  "Build a model using the complete dataset and write the produced model to a file determined by the 'model-file'-argument."},
	 {model_file,     undefined,    "model-file",  string,
	  "File name when writing a model to file (only applicable when using the 'build' or 'evaluate'-argument')."},
	 {evaluate,       $e,           "evaluate",    undefined,
	  "Evaluate the input dataset using a model read from a file described by the 'model-file'-argument."},
	 {proximity,      $p,           "proximity",   undefined,
	  "Generate a proximity matrix, which is available to support imputation of missing values (by supplying 'proximity' to the 'missing'-argument)."},

	 {progress,       undefined,    "progress",    {atom, dots},
	  "Show a progress bar while building a model. Available options include: 'dots', 'numeric' and 'none'. "},

	 {score,          undefined,    "score",       {atom, info},
	  "Defines the measure, which should be minimized, for evaluating the goodness of split points in each branch. Available options include: 'info' and 'gini', where 'info' denotes information entropy and 'gini' the gini-impurity."},
	 {classifiers,    $n,           "no-trees",    {integer, 10},
	  "Defines the number of classifiers (trees) to build."},

	 {max_depth,      undefined,    "max-depth",   {integer, 1000},
	  "Defines the maximum allowed depth for a single decision tree."},
	 {min_example,    undefined,    "min-examples",{integer, 1},
	  "Min number of examples allowed for splitting a node"},
	 
	 {random_rule,    undefined,    "random-rule", undefined,
	  "Rule or subset. The random weight is determined by the 'weight-factor'. If set to 1, 'rule' is always selected, if set to 0 a subset is always selected"},
	 {rule,           undefined,    "rule",        undefined,
	  "Build, at each node, k (determined by 'no-features') rules from m (determined by 'no-features') features. Thus including a decision based on [1, m] features at each branch."},
	 {combination,    undefined,    "combination", undefined,
	  "Generate k * (k - 1), where k is determined by 'no-features' combinations of features and evaluate the goodness of these at each split point. To allow for single features to be included, the attribute 'weight-factor' determines the probability of generating combinations (defaulting to 0.5)."},
	 {weka,           undefined,    "weka",        undefined,
	  "If none of the randomly sampled features provide any additional information, re-sample m (determined by 'no-features') attributes k=inf times."},
	 {resample,       undefined,    "resample",    undefined,
	  "If none of the randomly sampled features provide any additional information, re-sample m (determined by 'no-features') attributes k (determined by 'no-resamples') times."},
	 {sqrt,           undefined,    "sqrt",        undefined,
	  "Select sqrt(F) features, where F is the total number of features, at each split point and from these select the most promising. It is often a good strategy to use this option if your dataset has many (potentially) irrelevant predictors."},
	 {weighted,       undefined,    "weighted",    undefined,
	  "Calculate the most promising attributes before model induction, then bias the selection of features towards those that provide the highest a-priori information"},

	 {missing,        $m,           "missing",     {atom, random},
	  "Distributing missing values according to different strategies. Available options include: 'random', 'randomw', 'partitionw', 'partition', 'weighted', 'left', 'right' and 'ignore'. If 'random' is used, each example with missing values have an equal probability of be distributed over the left and right branch. If 'randomw' is selected, examples are randomly distributed over the left and right branch, but weighted towards the majority branch. If 'partition' is selected, each example is distributed equally over each branch. If 'partitionw' is selected, the example are distributed over each branch but weighted towards the majority branch. If 'weighted' is selected, each example is distributed over the majority branch. If 'left', 'right' or 'ignore' is selected, examples are distributed either to the left, right or is ignored, respectively."},

	 {distribute,     $d,           "distribute",  {atom, default},
	  "Distribute examples at each split according to different strategies. Available option include: 'default' or 'rulew'. If 'default' is selected, examples are distributed to either left or right. If 'rulew' is selected, fractions of each example are distributed according to how many antecedents each rule-node classifies the example."},

	 {bagging,        undefined,    "bagging",     undefined,
	  "To increase model diversity, a bootstrap replicate (i.e. sampling with replacement) of the original dataset is used when building each tree. [default]"},
	 {subagging,     undefined,    "subagging",    undefined,
	  "To increase model diversity and improve performance on large datasets, generate a subsample aggregate (i.e. a sample without replacement) from the original dataset."},

	 {weight_factor,  undefined,    "weight-factor", {float, 0.5},
	  "Used for controlling the randomness of the 'combination' and 'weighted'-arguments."},
	 {no_resamples,   undefined,    "no-resample", {integer, 6},
	  "Number of re-samples."},
	 {min_gain,       undefined,    "min-gain",    {float, 0},
	  "Minimum allowed gain for not re-sampling (if the 'resample'-argument is specified)."},
	 
	 {no_features,    undefined,    "no-features", {integer, -1},
	  "Number of features to inspect at each split. If set to -1 log(F)+1, where F denotes the total number of features, are inspected. The default value is usually a good compromise between diversity and performance."},

	 {output_predictions, $y,       "output-predictions", {boolean, false},
	  "Write the predictions to standard out."},
	 {variable_importance, $v, "variable-importance",     {integer, 0},
	  "Output the n most important variables calculated using the reduction in information averaged over all trees for each feature."},
	 {output,         $o,           "output",      {atom, default},
	  "Output format. Available options include: 'default' and 'csv'. If 'csv' is selected output is formated as a csv-file (see Example 5)"},

	 {log,            $l,           "log-level",   {atom, info},
	  "Log level. Available options include: 'debug', 'info', 'error' and 'none', where 'debug' output everything and 'none' output nothing.)"},
	 {log_target,     undefined,    "log-target",  {string, "stderr"},
	  "Write debug output to soure. Options include: 'stderr' or any valid file name, where 'stderr' writes output to standard output."}
	]).

main(Args) ->
    rr_example:init(),
    rr_ensemble:init(),
    random:seed(now()),

    Options = case getopt:parse(?CMD_SPEC, Args) of
		  {ok, Parsed} -> 
		      Parsed;
		  {error, {invalid_option, R}} ->
		      illegal(io_lib:format("unrecognized option '~s'", [R]));
		  {error, {missing_option_arg, R}} ->
		      illegal(io_lib:format("missing argument to option '~s'", [get_opt_name(R, ?CMD_SPEC)])); %%NOTE: fix
		  {error, _} ->
		      illegal("unknown error")
	      end,
    case any_opt([help, version, examples], Options) of
	help ->
	    show_help(),
	    halt();
	version ->
	    io:format(show_information()),
	    halt();
	examples ->
	    io:format(show_examples()),
	    halt();
	false ->
	    ok
    end,

    %% Initialize the Logger
    {Log, Logger} = create_logger(Options), 


    InputFile = get_opt(input_file, Options),
    Cores = get_opt(cores, Options),
    Output = create_output(Options),
    Missing = create_missing_values(Options),
    RunExperiment = create_experiment(Options),
    Progress = create_progress(Options),

    Logger(info, "Loading '~s' on ~p core(s)", [InputFile, Cores]),

    Csv = csv:binary_reader(InputFile),
    {Features, Examples0} = timer:tc(fun() -> rr_example:load(Csv, Cores) end),
    io:format("~p ~n", [Features]),
    halt(),
    Examples = rr_example:shuffle_dataset(Examples0),
    %% Dict = rr_example:generate_folds(Examples, 10),
    %% {Test, Train} = rr_example:merge_folds(Dict, 1),
    %% io:format("~p ~n ~p ~n ~p ~n", [Dict, Test, Train]),
    %% halt(),



    TotalNoFeatures = length(Features),
    NoFeatures = get_no_features(TotalNoFeatures, Options),
    Classifiers = get_opt(classifiers, Options),
    Score = create_score(Options),
    MaxDepth = get_opt(max_depth, Options),
    MinEx = get_opt(min_example, Options),
    Eval = create_brancher(NoFeatures, ordsets:from_list(Features), Examples, Missing, Score, Options),
    Bagging = create_bagger(Options),
    Distribute = create_distribute(Options),

    Conf = #rr_conf{
	      cores = Cores,
	      score = Score,
	      prune = rr_tree:example_depth_stop(MinEx, MaxDepth),
	      branch = Eval,
	      bagging = Bagging,
	      progress = Progress,
	      split = fun rr_tree:random_split/4,
	      distribute = Distribute,
	      missing_values = Missing,
	      base_learner = {Classifiers, rr_tree},
	      no_features = TotalNoFeatures,
	      output = Output,
	      log = Logger,
	      payload = undefined
	     },

    Logger(info, "Building model using ~p trees and ~p features", [Classifiers, NoFeatures]),
    Output(start, now()),
    Then = now(),
    RunExperiment(Features, Examples, Conf, Options),
    Time = timer:now_diff(now(), Then) / 1000000,

    Output(parameters, [
			{file, InputFile},
			{classifiers, Classifiers},
			{no_features, NoFeatures},
			{total_no_features, TotalNoFeatures},
			{examples, rr_example:count(Examples)},
			{time, Time}			 
		       ]),
    Output('end', now()),
    Logger(debug, "Input parameters ~p", [Options]),
    Logger(info, "Model built in ~p second(s)", [Time]),
    rr_log:stop(Log).

output_variable_importance(Model, #rr_conf{output=Output} = Conf, Options) ->
    case get_opt(variable_importance, Options) of
	No when No > 1 ->
	    VariableImportance = rr_ensemble:variable_importance(Model, Conf),
	    Sorted = lists:reverse(lists:keysort(2, dict:to_list(VariableImportance))),
	    Output(vi, {Sorted, 1, No});	    
	_No ->
	    ok
    end.

output_predictions(Data, #rr_conf{output=Output}, Options) ->
    case get_opt(output_predictions, Options) of
	true -> Output(predictions, Data);
	false -> ok
    end.

output_evaluation(Data, #rr_conf{output=Output}, Options) ->
    Output(evaluation, [{file, get_opt(input_file, Options)}] ++ Data).
	     
	
run_proximity(Features, Examples, Conf) ->
    rr_proximity:init(),
    Model = rr_ensemble:generate_model(Features, Examples, Conf),
    rr_proximity:generate_proximity(Model, Examples, Conf).

run_split(Features, Examples, #rr_conf{output=Output} = Conf, Options) ->
    Split = get_opt(ratio, Options),
    {Train, Test} = rr_example:split_dataset(Examples, Split),
    Model = rr_ensemble:generate_model(Features, Train, Conf),

    
    Dict = rr_ensemble:evaluate_model(Model, Test, Conf),
    Evaluation = evaluate(Dict, rr_example:count(Test)),
    
    Output(method, {"Split", Split}),
    output_variable_importance(Model, Conf, Options),
    output_predictions(Test, Conf, Options),
    output_evaluation(Evaluation, Conf, Options).

    
    
run_build_process(Features, Examples, Conf, Options) ->
    Model = rr_ensemble:generate_model(Features, Examples, Conf),
    rr_ensemble:save_model(Model, get_opt(model_file, Options)).

run_cross_validation(Features, Examples, #rr_conf{output=Output} = Conf, Options) ->
    Folds = get_opt(folds, Options),
    Avg = rr_example:cross_validation(
	    fun(Train0, Test0, Fold) ->
		    io:format(standard_error, "*** Fold ~p *** ~n", [Fold]),
		    Output(method, {"Fold", Fold}),
		    M = rr_ensemble:generate_model(Features, Train0, Conf),
		    D = rr_ensemble:evaluate_model(M, Test0, Conf),
		    Evaluate = evaluate(D, rr_example:count(Test0)),
		    output_evaluation(Evaluate, Conf, Options),
		    Evaluate			
	    end, Folds, Examples),
    Output(method, {"Fold", average}),
    output_evaluation(average_cross_validation(Avg, Folds, [accuracy, auc, brier], []), Conf, Options),
    output_predictions(Examples, Conf, Options).

average_cross_validation(_, _, [], Acc) ->
    lists:reverse(Acc);
average_cross_validation(Avg, Folds, [H|Rest], Acc) ->
    A = lists:foldl(fun (Measures, Sum) ->
			    case lists:keyfind(H, 1, Measures) of
				{H, _, Auc} ->
				    Sum + Auc;
				{H, O} ->
				    Sum + O
			    end
		    end, 0, Avg) / Folds,
    average_cross_validation(Avg, Folds, Rest, [{H, A}|Acc]).
		    
evaluate(Dict, NoTestExamples) ->
    Accuracy = rr_eval:accuracy(Dict),
    Auc = rr_eval:auc(Dict, NoTestExamples),
    AvgAuc = lists:foldl(fun
			     ({_, No, A}, Sum) -> 
				 Sum + No/NoTestExamples*A;
			     ({_, 'n/a', _}, Sum) -> 
				 Sum
			 end, 0, Auc),
    Precision = rr_eval:precision(Dict),
    Brier = rr_eval:brier(Dict, NoTestExamples),
    [{accuracy, Accuracy}, {auc, Auc, AvgAuc}, {precision, Precision}, {brier, Brier}].

create_bagger(Options) ->
    case any_opt([subagging, bagging], Options) of
	subagging ->
	    fun rr_example:subset_aggregate/1;
	_ ->
	    fun rr_example:bootstrap_aggregate/1
    end.

create_distribute(Options) ->	
    case get_opt(distribute, Options) of
	default ->
	    fun rr_example:distribute/2;
	rulew ->
	    fun rr_rule:distribute_weighted/2;
	Other ->
	    illegal_option("distribute", Other)
    end.

create_logger(Options) ->
    case get_opt(log_target, Options) of
	"stderr" ->
	    Log0 = rr_log:new(std_err, get_opt(log, Options)),
	    MaxLevel = rr_log:to_number(get_opt(log, Options)),
	    {Log0, fun (Level, Message, Params) ->
			   Level0 = rr_log:to_number(Level),
			   if Level0 > MaxLevel ->
				   ok;
			      true -> rr_log:log(Log0, Level, Message, Params)
			   end
		   end};
	Target ->
	    Log0 = rr_log:new(Target, get_opt(log, Options)),
	    MaxLevel = rr_log:to_number(get_opt(log, Options)),
	    {Log0, fun (Level, Message, Params) ->
			   Level0 = rr_log:to_number(Level),
			   if Level0 > MaxLevel ->
				   ok;
			      true -> rr_log:log(Log0, Level, Message, Params)
			   end
		   end}
    end.

create_missing_values(Options) ->
    case get_opt(missing, Options) of
	random ->
	    fun rr_missing:random/5;
	randomw ->
	    fun rr_missing:random_weighted/5;
	weighted ->
	    fun rr_missing:weighted/5;
	partition ->
	    fun rr_missing:random_partition/5;
	wpartition ->
	    fun rr_missing:weighted_partition/5;
	proximity ->
	    fun rr_missing:proximity/5;
	right ->
	    fun rr_missing:right/5;
	left ->
	    fun rr_missing:left/5;
	ignore ->
	    fun rr_missing:ignore/5;
	Other ->
	    illegal_option("missing", Other)
    end.

create_output(Options) ->
    case get_opt(output, Options) of
	default ->
	    rr_result:default();
	csv ->
	    rr_result:csv();
	Other ->
	    illegal_option("output", Other)
    end.
	    

create_experiment(Options) ->
    case any_opt([cv, split, build, evaluate, proximity], Options) of
	split ->
	    fun run_split/4;
	cv ->
	    fun run_cross_validation/4;
	build ->
	    fun run_build_process/4;
	evaluate ->
	    ok;
	false ->		
	    illegal("No method selected. Please use either 'split', 'cross-validate' or 'build' argument")
    end.

create_progress(Options) ->
    case get_opt(progress, Options) of
	dots ->
	    fun(_, _) -> io:format(standard_error, "..", []) end;
	numeric ->
	    fun(Id, T) -> io:format(standard_error, "~p/~p.. ", [Id, T]) end;
	none ->
	    fun(_, _) -> ok end;
	Other ->
	    illegal_option("progress", Other)
    end.

create_score(Options) ->
    case get_opt(score, Options) of
	info ->
	    rr_tree:info();
	gini ->
	    rr_tree:gini();
	Other ->
	    illegal_option("score", Other)
		
    end.

get_no_features(TotalNoFeatures, Options) ->
    case any_opt([sqrt, no_features], Options) of
	false ->
	    case get_opt(no_features, Options) of
		X when X =< 0 ->
		    round(math:log(TotalNoFeatures)/math:log(2)) + 1;
		X ->
		    X
	    end;
	sqrt ->
	    round(math:sqrt(TotalNoFeatures))
    end.

create_brancher(NoFeatures, _Features, _Examples, _Missing, _Score, Options) ->
    case any_opt([weka, resample, weighted, combination, rule, random_rule], Options) of
	weka ->
	    rr_branch:weka(NoFeatures);
	resample ->
	    NoResamples = get_opt(no_resamples, Options),
	    MinGain = get_opt(min_gain, Options),
	    rr_branch:resampled(NoResamples, NoFeatures, MinGain);
	combination ->
	    Factor = get_opt(weight_factor, Options),
	    rr_branch:random_correlation(NoFeatures, Factor);
	rule ->
	    rr_branch:rule(NoFeatures); %% TODO: user selected rule score function
	random_rule ->
	    Factor = get_opt(weight_factor, Options),
	    rr_branch:random_rule(NoFeatures, Factor);
	false -> 
	    rr_branch:subset(NoFeatures)
    end.

illegal() ->
    getopt:usage(?CMD_SPEC, "rr"),
    halt().

illegal(Argument, Error) ->
    illegal(Argument, Error, []),
    halt().

illegal(Argument, Error, Args) ->
    io:format(standard_error, "rr: '~s': ~s. ~n", [Argument, io_lib:format(Error, Args)]),
    halt().

illegal_option(Argument, Option) ->
    illegal(io_lib:format("unrecognized option '~s' for '~s'", [Option, Argument])).

illegal(Error) ->
    io:format(standard_error, "rr: ~s. ~nPlease consult the manual.~n", [Error]),
    halt().

default_illegal(Out) ->
    fun() ->
	    illegal(Out)
    end.

show_help() ->
    getopt:usage(?CMD_SPEC, "rr"),
    io:format(standard_error, "~s

VERSION
================================
~s", [show_examples(), show_information()]).

show_examples() ->
    "EXAMPLES
================================
Example 1: 10-fold cross validation 'car' dataset:
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
  > for f in data/*.txt; do ./rr -i \"$f\" -s -r 0.7 -o csv >> result.csv; done~n".

get_opt(Arg, Fun1, {Options, _}) ->	
    case lists:keyfind(Arg, 1, Options) of
	{Arg, Ws} ->
	    Ws;
	false -> 
	    Fun1()
    end.

get_opt(Arg, Options) ->
    get_opt(Arg, default_illegal(io_lib:format("unrecognized argument '~s'", [Arg])), Options).

get_opt_name(Name, []) ->
    Name;
get_opt_name(Name, [{RealName, _, Long, _Default, _Descr}|Rest]) ->
    if Name == RealName ->
	    Long;
       true ->
	    get_opt_name(Name, Rest)
    end.
    
any_opt([], _) ->
    false;
any_opt([O|Rest], Options) ->
    case has_opt(O, Options) of
	true ->
	    O;
	false ->
	    any_opt(Rest, Options)
    end.

has_opt(Arg, {Options, _ }) ->
    lists:any(fun (K) ->
		      K == Arg
	      end, Options).
    

show_information() -> 
    io_lib:format("rr (Random Rule Learner) ~s.~s.~s (build date: ~s)
Copyright (C) 2013+ ~s

Written by ~s ~n", [?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?DATE, ?AUTHOR, ?AUTHOR]).
