%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created :  8 Apr 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
-module(rf_branch).
-export([
	 random/0,
	 resampled/3,
	 weka/1,
	 all/0,

	 subset/1,
	 random_subset/2,

	 correlation/1,
	 random_correlation/2,
	 rule/3,
	 random_rule/4,

	 random_examples/2	 
	]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @headerfile "rr_tree.hrl"
%-include("rr.hrl").

%% @doc resamples n new features k times if arg max gain(Features)
-spec resampled(integer(), integer(), float()) -> branch_fun().
resampled(NoResamples, NoFeatures, Delta) ->
    fun (Features, Examples, Total, Conf) ->
	    %% todo: calculate total no features (once)
	    resampled_subset_branch_split(Features, Examples, Total, Conf, NoResamples, Delta, NoFeatures)
    end.

%% @private resample features
resampled_subset_branch_split(_, _, _, #rf_tree{no_features=NoFeatures}, NoResamples, _, _) when NoFeatures =< 0; NoResamples =< 0 ->
    no_information;
resampled_subset_branch_split(Features, Examples, Total, Conf, NoResamples, Delta, Log) ->
    #rf_tree{score = ScoreFun, split = Split, distribute = Distribute, missing_values = Missing, no_features = NoFeatures} = Conf,
    Features0 = if NoFeatures =< Log ->
			Features;
		   true ->
			rr_example:random_features(Features, Log)
		end,

    Cand = rr_example:best_split(Features0, Examples, Total, ScoreFun, Split, Distribute, Missing),
    {Score, _, _} = Cand#rr_candidate.score,
    Gain = (Total*rr_tree:entropy(Examples)) - Score, 
    if  Gain =< Delta ->
	    resampled_subset_branch_split(ordsets:subtract(Features, ordsets:from_list(Features0)), 
					  Examples, Total, Conf#rf_tree{no_features=NoFeatures - Log}, 
					  NoResamples - 1, Delta, Log);
	true ->
	    Cand
    end.

%% @doc resample features similar to Weka for ignoring non-informative features
-spec weka(integer()) -> branch_fun().
weka(NoFeatures) ->
    fun(Features, Examples, Total, Conf) ->
	    weka_branch_split(Features, Examples, Total, Conf, NoFeatures)
    end.

%% @private
weka_branch_split(_, _, _, #rf_tree{no_features=NoTotal}, _) when NoTotal =< 0 ->
    no_information;
weka_branch_split(Features, Examples, Total, Conf, NoFeatures) ->
    #rf_tree{score = ScoreFun, split = Split, distribute = Distribute, missing_values = Missing, no_features = NoTotal} = Conf,
    Features0 = if NoTotal =< NoFeatures ->
			Features;
		   true -> 
			rr_example:random_features(Features, NoFeatures)
		end,
    Cand = rr_example:best_split(Features0, Examples, Total, ScoreFun, Split, Distribute, Missing),
    {Score, _, _} = Cand#rr_candidate.score,
    Gain = (Total*rr_tree:entropy(Examples)) - Score,
    if Gain =< 0.0 ->
	    weka_branch_split(ordsets:subtract(Features, ordsets:from_list(Features0)),
			      Examples, Total, Conf#rf_tree{no_features=NoTotal - NoFeatures}, NoFeatures);
       true ->
	    Cand
    end.

%% @doc evaluate a subset of n random features
-spec subset(integer()) -> branch_fun().
subset(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
	    Features0 = rr_example:random_features(Features, NoFeatures),
	    rr_example:best_split(Features0, Examples, Total, Score, Split, Distribute, Missing)
    end.

%% @doc evaluate the combination of (n*n)-1 features
-spec correlation(integer()) -> branch_fun().
correlation(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
	    FeaturesA = rr_example:random_features(Features, NoFeatures),
	    FeaturesB = rr_example:random_features(Features, NoFeatures),
	    
	    Combination = [{combined, A, B} || A <- FeaturesA, B <- FeaturesB, A =/= B],
	    rr_example:best_split(Combination, Examples, Total, Score, Split, Distribute, Missing)
    end.

%% @doc tandomly pick either a subset brancher or a correlation brancher
-spec random_correlation(integer(), float()) -> branch_fun().
random_correlation(NoFeatures, Fraction) ->
    Corr = correlation(NoFeatures),
    Sub = subset(NoFeatures),
    fun (Features, Examples, Total, Conf) ->
	    Random = random:uniform(),
	    if Random =< Fraction ->
		    Corr(Features, Examples, Total, Conf);
	       true ->
		    Sub(Features, Examples, Total, Conf)
	    end
    end.

%% @doc evalate one randomly selected feature (maximum diversity)
-spec random() -> branch_fun().
random() ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing, no_features = NoFeatures} = Conf,
	    Feature = lists:nth(random:uniform(NoFeatures), Features),
	    rr_example:best_split([Feature], Examples, Total, Score, Split, Distribute, Missing)
    end.

%% @doc evaluate all features to find the best split point
-spec all() -> branch_fun().
all() ->
    fun(Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
	    rr_example:best_split(Features, Examples, Total, Score, Split, Distribute, Missing)
    end.

%% @doc generate a rule at each branch
-spec rule(integer(), integer(), score_fun()) -> branch_fun().
rule(NoFeatures, NoRules, RuleScore) ->
    fun (Features, Examples, Total, Conf) ->
	    NewConf = Conf#rf_tree{split = fun rf_tree:deterministic_split/4},
	    rf_rule:best(Features, Examples, Total, NewConf, NoFeatures, NoRules, RuleScore)
    end.

%% @doc randomly pick a subset-brancher or a rule-bracher at each node
-spec random_rule(integer(), integer(), score_fun(), float()) -> branch_fun().
random_rule(NoFeatures, NoRules, RuleScore, Prob) ->
    Rule = rule(NoFeatures, NoRules, RuleScore),
    Sub = subset(NoFeatures),
    fun (Features, Examples, Total, Conf) ->
	    Random = random:uniform(),
	    if Random =< Prob ->
		    Rule(Features, Examples, Total, Conf);
	       true ->
		    Sub(Features, Examples, Total, Conf)
	    end
    end.

random_examples(NoFeatures, NoExamples) ->
    Log = subset(NoFeatures),
    fun (Features, Examples, Total, Conf) ->
	    if Total =< NoExamples ->
		    Log(Features, Examples, Total, Conf);
	       true ->
		    random_examples(Features, Examples, Total, Conf, NoExamples)
	    end
    end.

random_examples(Features, Examples, Total, Conf, NoExamples) ->
    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
    Examples0 = rr_examples:random_examples(Examples, NoExamples),
    rr_example:best_split(Features, Examples0, NoExamples, Score, Split, Distribute, Missing).


random_subset(NoFeatures, Variance) ->
    fun (Features, Examples, Total, Conf) ->
	    random_subset(Features, Examples, Total, Conf, NoFeatures, Variance)
    end.

random_subset(Features, Examples, Total, Conf, NoFeatures, Variance) ->
    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
    Random0 = random:uniform(),
    Random1 = random:uniform(),
	
    NewVariance = Random0 * Variance * if Random1 > 0.5 -> 1; true -> 1 end,
    NewNoFeatures = case round(NoFeatures + NewVariance) of
			X when X > length(Features) -> length(Features);
			X when X < 1 -> 1;
			X -> X
		    end,
    NewFeatures = rr_example:random_features(Features, NewNoFeatures),
    rr_example:best_split(NewFeatures, Examples, Total, Score, Split, Distribute, Missing).
    
    
    
