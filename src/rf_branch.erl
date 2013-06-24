%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created :  8 Apr 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
-module(rf_branch).
-export([
	 
	 %% standard approaches
	 random/0,
	 all/0,
	 subset/1,
	 random_subset/2,

	 %% multi-feature approaches
	 correlation/1,
	 random_correlation/2,
	 rule/3,
	 random_rule/4,

	 %% example sampling approaches
	 sample_examples/3,

	 %% increased strength approaches
	 depth/1,
	 depth_rule/3,

	 %% resampling approahces
	 weka/1,
	 resample/3,

	 %% significance approaches
	 random_chisquare/2,
	 chisquare/2,
	 chisquare_decrease/3,

	 %% util
	 unpack/1,
	 redo/2,
	 random/3	 
	]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @doc evaluate a subset of n random features
-spec subset(integer()) -> branch_fun().
subset(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
	    NewFeatures = rr_example:random_features(Features, NoFeatures),
	    {rr_example:best_split(NewFeatures, Examples, Total, Score, Split, Distribute, Missing), NewFeatures}
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
    random(Corr, Sub, Fraction).

%% @doc evalate one randomly selected feature (maximum diversity)
-spec random() -> branch_fun().
random() ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, 
		     missing_values = Missing, no_features = NoFeatures} = Conf,
	    Feature = [lists:nth(random:uniform(NoFeatures), Features)],
	    rr_example:best_split(Feature, Examples, Total, Score, Split, Distribute, Missing)
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
    random(Rule, Sub, Prob).

sample_examples(NoFeatures, Size, Sigma) ->
    Do = sample_examples(NoFeatures, Size),
    Redo = sample_examples_significance(NoFeatures, Size, Sigma),
    redo(Do, Redo).

sample_examples(_NoFeatures, Size) when Size >= 1.0 ->
    fun (_, _, _, _) ->
	    {Size, {invalid_subset, []}}
    end;
sample_examples(NoFeatures, Size) ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, missing_values = Missing} = Conf,
	    FeatureSubset = rr_example:random_features(Features, NoFeatures),
	    Subset = sample_sane_examples(Examples, Size, Size, Total),
	    #rr_candidate{feature=F} = rr_example:best_split(FeatureSubset, Subset, rr_example:count(Subset), Score, Split, Distribute, Missing),
	    ExSplit = rr_example:split_feature_value(F, Examples, Distribute, Missing),
	    Best = #rr_candidate{feature = F,
				 split = ExSplit,
				 score = Score(ExSplit, rr_example:count(Examples))},
	    {Size, {Best, []}}
    end.

sample_sane_examples(Examples, Size, _,  _) when Size >= 1 ->
    Examples;
sample_sane_examples(Examples, Size, Step, Total) ->
    if Total * Size < 2 ->
	    Examples;
       true ->
	    case rr_example:subset(Examples, Size) of
		Subset when length(Subset) > 1 -> Subset;
		_ ->
		    sample_sane_examples(Examples, Size+Step, Step, Total)
	    end
    end.

sample_examples_significance(NoFeatures, Size, Sigma) ->
    fun (#rr_candidate{split=Split}, NewSize, Examples, Total) ->
	    S = rr_estimator:chisquare(Split, Examples, Total),
	    if S < Sigma ->
		    Inc = NewSize + Size,
		    {true, sample_examples(NoFeatures, Inc), sample_examples_significance(NoFeatures, Inc, Sigma)};
	       true ->
		    false
	    end;
	(invalid_subset, _, _, _) ->
	    no_information
    end.

%% @doc subset with a slight variance in number of sampled features
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
    
depth(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{score = Score, split = Split, distribute = Distribute, 
		     missing_values = Missing, depth = Depth} = Conf,
	    DepthRatio = 1 / (math:log(3+Depth)),
	    NewNoFeatures = case round(NoFeatures * DepthRatio) of
				X when X =< 1 -> 1;
				X when X >= NoFeatures -> NoFeatures;
				X -> X
			    end,
	    NewFeatures = rr_example:random_features(Features, NewNoFeatures),
	    rr_example:best_split(NewFeatures, Examples, Total, Score, Split, Distribute, Missing)
    end.
				       
depth_rule(NoFeatures, NoRules, RuleScore) ->
    Rule = rule(NoFeatures, NoRules, RuleScore),
    Subset = subset(NoFeatures),
    fun (Features, Examples, Total, Conf) ->
	    #rf_tree{depth=Depth} = Conf,
	    if Depth >= NoFeatures -> %% todo: base on what?
		    Rule(Features, Examples, Total, Conf);
	       true ->
		    Subset(Features, Examples, Total, Conf)
	    end
    end.

%% @doc
%% resample a decreasing number of features each time an
%% insignificant feature is found, until only one feature is inspected
%% @end
chisquare_decrease(NoFeatures, Rate, Sigma) ->
    Sample = redo_curry(NoFeatures, subset(NoFeatures)),
    redo(Sample, chisquare_decrease_resample(Sample, Rate, Sigma)).
    
chisquare_decrease_resample(_Sample, Rate, Sigma) ->
    fun (#rr_candidate{split=Split}, NoFeatures, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    if Significance < Sigma ->
		    NewNoFeatures = if NoFeatures > 1 -> round(NoFeatures * Rate); true -> 1 end,
		    NewSample = redo_curry(NewNoFeatures, subset(NewNoFeatures)),
		    {true, NewSample, chisquare_decrease_resample(NewSample, Rate, Sigma)};
	       true ->
		    false
	    end
    end.

%% @doc randomly select either chisquare or subset
random_chisquare(NoFeatures, Sigma) ->
    Chi = chisquare(NoFeatures, Sigma),
    Sub = subset(NoFeatures),
    random(Chi, Sub, 0.5).

%% @doc resample features if chi-square significance is lower than Sigma
chisquare(NoFeatures, Sigma) ->
    Sample = redo_curry(NoFeatures, subset(NoFeatures)),
    Resample = chisquare_resample(Sample, Sigma),
    redo(Sample, Resample).
		
chisquare_resample(Sample, Sigma) ->
    fun (#rr_candidate{split=Split}, _, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    if Significance < Sigma ->
		    {true, Sample, chisquare_resample(Sample, Sigma)};
	       true ->
		    false
	    end
    end.

%% @doc resample n features m times if gain delta
resample(NoFeatures, NoResamples, Delta) ->
    Sample = redo_curry(NoFeatures, subset(NoFeatures)),
    Resample = simple_resample(Sample, NoResamples, Delta),
    redo(Sample, Resample).

simple_resample(_, 0, _) ->
    fun (_, _, _, _) -> false end;
simple_resample(Sample, NoResamples, Delta) ->
    fun (#rr_candidate{score = {Score, _, _}}, _, Examples, Total) ->
	    Gain = (Total * rr_estimator:entropy(Examples)) - Score,
	    if Gain =< Delta ->
		    {true, Sample, simple_resample(Sample, NoResamples - 1, Delta)};
	       true ->
		    false
	    end
    end.

%% @doc resample 1 feature if no informative features is found
weka(NoFeatures) ->
    Sample = redo_curry(NoFeatures, subset(NoFeatures)),
    Resample = weka_resample(Sample),
    redo(Sample, Resample).

weka_resample(_Sample) ->
    fun (#rr_candidate{score = {Score, _, _}}, _, Examples, Total) ->
	    Gain = (Total * rr_estimator:entropy(Examples)) - Score,
	    if Gain =< 0.0 ->
		    NewSample = redo_curry(1, subset(1)),
		    {true, NewSample, weka_resample(NewSample)};
	       true ->
		    false
	    end
    end.

%% @doc
%% Generic function for performing resampling. The first argument - Sample - is used for sampling
%% features and for finding a candidate. The function should return a tuple with the number of sampled
%% features, and {BestCandidate, SampledFeatures}. The second argument - resample - should be used for
%% determining if resampling is needed. This function returns {true, NewSample, NewResample} if
%% no informative feature is found, o/w false. 
%% @end
redo(Do, Redo) ->
    fun (Features, Examples, Total, Conf) ->
	    redo(Features, Examples, Total, Conf, length(Features), #rr_candidate{score = {inf}}, Do, Redo)
    end.

redo([], _, _, _, _, _, _, _) ->
    no_information;
redo(Features, Examples, Total, Conf, TotalNoFeatures, Prev, Do, Redo) ->
    {NoFeatures, {Best, NewFeatures}} = Do(Features, Examples, Total, Conf),
    case Redo(Best, NoFeatures, Examples, Total) of
	{true, NewDo, NewRedo} ->
	    redo(ordsets:subtract(Features, ordsets:from_list(NewFeatures)),
		 Examples, Total, Conf, 
		 if is_integer(NoFeatures) ->
			 TotalNoFeatures - NoFeatures;
		    true ->
			 TotalNoFeatures
		 end, Best, NewDo, NewRedo); 
	false ->
	    if element(1, Best#rr_candidate.score) < element(1, Prev#rr_candidate.score) ->
		    {Best, NewFeatures};
	       true ->
		    {Prev, NewFeatures}
	    end;
	no_information ->
	    no_information
    end.

%% @doc call either One or Two randomly according to T
random(One, Two, T) ->
    fun (Features, Examples, Total, Conf) ->
	    R = random:uniform(),
	    if R =< T ->
		    One(Features, Examples, Total, Conf);
	       true ->
		    Two(Features, Examples, Total, Conf)
	    end
    end.
    
%% @doc wrap sample-fun Fun to return the number of sampled features
redo_curry(NoFeatures, Fun) ->
    fun (Features, Examples, Total, Conf) ->
	    {NoFeatures, Fun(Features, Examples, Total, Conf)}
    end.

%% @doc unpack a candidate
unpack({Candidate, _Features}) ->
    Candidate;
unpack(Candidate) ->
    Candidate.
