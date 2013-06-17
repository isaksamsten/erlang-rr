%%% @author Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created :  8 Apr 2013 by Isak Karlsson <isak@dhcp-159-53.dsv.su.se>
-module(rf_branch).
-export([
	 random/0,
	 weka/1,
	 all/0,

	 subset/1,
	 random_subset/2,

	 correlation/1,
	 random_correlation/2,
	 rule/3,
	 random_rule/4,

	 random_examples/2,

	 depth/1,
	 depth_rule/3,

	 chi_square/1,
	 resample/3,
	 chisquare/2,

	 unpack/1
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

random_examples(Features, Examples, _Total, Conf, NoExamples) ->
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

chi_square(NoFeatures) ->
    Subset = subset(NoFeatures),
    fun (Features, Examples, Total, Conf) ->
	    Cand = Subset(Features, Examples, Total, Conf),
	    Split = Cand#rr_candidate.split,
	    K = rr_estimator:chisquare(Split, Examples, Total),
	    if K < 0.05 ->
		    no_information;
	       true ->
		    Cand
	    end	    
    end.       

chisquare(NoFeatures, Sigma) ->
    Sample = sample_wrap(NoFeatures, subset(NoFeatures)),
    Resample = chisquare_resample(Sample, Sigma),
    resample(Sample, Resample).

chisquare_resample(Sample, Sigma) ->
    fun (#rr_candidate{split=Split}, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    if Significance < Sigma ->
%		    io:format("~p is not significant. ~n", [Significance]),
		    {true, Sample, chisquare_resample(Sample, Sigma)};
	       true ->
		    false
	    end
    end.
		

%% @doc resample n features m times if gain =< delta
resample(NoFeatures, NoResamples, Delta) ->
    Sample = sample_wrap(NoFeatures, subset(NoFeatures)),
    Resample = simple_resample(Sample, NoResamples, Delta),
    resample(Sample, Resample).

simple_resample(_, 0, _) ->
    fun (_, _, _) -> false end;
simple_resample(Sample, NoResamples, Delta) ->
    fun (#rr_candidate{score = {Score, _, _}}, Examples, Total) ->
	    Gain = (Total * rr_estimator:entropy(Examples)) - Score,
	    if Gain =< Delta ->
		    {true, Sample, simple_resample(Sample, NoResamples - 1, Delta)};
	       true ->
		    false
	    end
    end.

%% @doc resample 1 feature if no informative features is found
weka(NoFeatures) ->
    Sample = sample_wrap(NoFeatures, subset(NoFeatures)),
    Resample = weka_resample(Sample),
    resample(Sample, Resample).

weka_resample(Sample) ->
    fun (#rr_candidate{score = {Score, _, _}}, Examples, Total) ->
	    Gain = (Total * rr_estimator:entropy(Examples)) - Score,
	    if Gain =< 0.0 ->
		    NewSample = sample_wrap(1, subset(1)),
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
resample(Sample, Resample) ->
    fun (Features, Examples, Total, Conf) ->
	    resample(Features, Examples, Total, Conf, length(Features), Sample, Resample) %% optimize
    end.

resample([], _, _, _, _, _, _) ->
    no_information;
resample(Features, Examples, Total, Conf, TotalNoFeatures, Sample, Resample) ->
    {NoFeatures, {Best, NewFeatures}} = Sample(Features, Examples, Total, Conf),
    case Resample(Best, Examples, Total) of
	{true, NewSample, NewResample} ->
	    resample(ordsets:subtract(Features, ordsets:from_list(NewFeatures)),
		     Examples, Total, Conf, TotalNoFeatures - NoFeatures, NewSample, NewResample); 
	false ->
	    {Best, NewFeatures}
    end.
    
%% @doc wrap sample-fun Fun to return the number of sampled features
sample_wrap(NoFeatures, Fun) ->
    fun (Features, Examples, Total, Conf) ->
	    {NoFeatures, Fun(Features, Examples, Total, Conf)}
    end.

%% @doc unpack a candidate
unpack({Candidate, _Features}) ->
    Candidate;
unpack(Candidate) ->
    Candidate.
