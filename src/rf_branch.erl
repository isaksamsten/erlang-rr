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
	 subset/0,
	 random_subset/2,

	 %% multi-feature approaches
	 correlation/0,
	 random_correlation/1,
	 rule/2,
	 random_rule/3,
	 choose_rule/2,

	 %% example sampling approaches
	 sample_examples/3,

	 %% increased strength approaches
%	 depth_rule/2,

	 %% resampling approahces
	 weka/0,
	 resample/2,

	 %% significance approaches
	 random_chisquare/1,
	 randomly_resquare/2,
	 chisquare/1,
	 chisquare_decrease/2,

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

-spec subset() -> branch_fun().
subset() ->
    fun subset/5.

-spec correlation() -> branch_fun().
correlation() ->
    fun correlation/5.

-spec random() -> branch_fun().
random() ->
    fun random/5.

-spec all() -> branch_fun().
all() ->
    fun all/5.


%% @doc evaluate a subset of n random features
-spec subset(features(), examples(), integer(), #rr_example{}, #rf_tree{}) -> branch().
subset(Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score, 
       split = Split, 
       distribute = Distribute, 
       missing_values = Missing,
       no_features = NoFeatures
      } = Conf,
    NewFeatures = rr_example:random_features(Features, NoFeatures(length(Features))),
    {rr_example:best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing), NewFeatures}.

%% @doc evaluate the combination of (n*n)-1 features
-spec correlation(features(), examples(), integer(), #rr_example{}, #rf_tree{}) -> branch().
correlation(Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score, 
       split = Split,
       distribute = Distribute, 
       missing_values = Missing,
       no_features = NoFeatures       
      } = Conf,
    Select = NoFeatures(length(Features)),
    FeaturesA = rr_example:random_features(Features, Select),
    FeaturesB = rr_example:random_features(Features, Select),

    Combination = [{combined, A, B} || A <- FeaturesA, B <- FeaturesB, A =/= B],
    rr_example:best_split(ExConf, Combination, Examples, Total, 
			  Score, Split, Distribute, Missing).

%% @doc tandomly pick either a subset brancher or a correlation brancher
-spec random_correlation(float()) -> branch_fun().
random_correlation(Fraction) ->
    Corr = correlation(),
    Sub = subset(),
    random(Corr, Sub, Fraction).

%% @doc evalate one randomly selected feature (maximum diversity)
-spec random(features(), examples(), integer(), #rr_example{}, #rf_tree{}) -> branch().
random (Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score, 
       split = Split, 
       distribute = Distribute, 
       missing_values = Missing
      } = Conf,
    NoFeatures = length(Features),
    Feature = [lists:nth(random:uniform(NoFeatures), Features)],
    rr_example:best_split(ExConf, Feature, Examples, Total, 
			  Score, Split, Distribute, Missing).    

%% @doc evaluate all features to find the best split point
-spec all(features(), examples(), integer(), #rr_example{}, #rf_tree{}) -> branch().
all (Features, Examples, Total, ExConf, Conf) ->
    #rf_tree {
       score = Score, 
       split = Split,
       distribute = Distribute,
       missing_values = Missing
      } = Conf,
    rr_example:best_split(ExConf, Features, Examples, Total, 
			  Score, Split, Distribute, Missing).

%% @doc generate a rule at each branch
-spec rule(integer(), score_fun()) -> branch_fun().
rule(NoRules, RuleScore) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    NewConf = Conf#rf_tree {
			split = fun rf_tree:deterministic_split/5
		       },
	    #rf_tree{
	       no_features = NoFeatures
	      } = Conf,
	    FeatureCount = length(Features),
	    rf_rule:best(Features, Examples, Total, ExConf, NewConf, 
			 NoFeatures(FeatureCount), NoRules(FeatureCount), RuleScore)
    end.

%% @doc randomly pick a subset-brancher or a rule-bracher at each node
-spec random_rule(integer(), score_fun(), float()) -> branch_fun().
random_rule(NoRules, RuleScore, Prob) ->
    Rule = rule(NoRules, RuleScore),
    Sub = subset(),
    random(Rule, Sub, Prob).

%% @doc choose either rule or subset depending on which is best
choose_rule(NoRules, RuleScore) ->
    Rule = rule(NoRules, RuleScore),
    Sub = subset(),
    n([Rule, Sub], fun sig/3).

%% @doc choose the candidate which is most significant
sig([#rr_candidate{split = As}=A, #rr_candidate{split=Bs} = B], Examples, Total) ->
    Achi = rr_estimator:chisquare(As, Examples, Total),
    Bchi = rr_estimator:chisquare(Bs, Examples, Total),
    if Achi > Bchi ->
	    A;
       true -> 
	    B
    end.

%% @doc sample a set of examples of Size, if we can 
%% find a significant split use it o/w retry @end
sample_examples(NoFeatures, Size, Sigma) ->
    Do = sample_examples(NoFeatures, Size),
    Redo = sample_examples_significance(NoFeatures, Size, Sigma),
    redo(Do, Redo).

sample_examples(_NoFeatures, Size) when Size >= 1.0 ->
    fun (_, _, _, _) ->
	    {Size, {invalid_subset, []}}
    end;
sample_examples(NoFeatures, Size) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    #rf_tree{
	       score = Score, 
	       split = Split, 
	       distribute = Distribute, 
	       missing_values = Missing
	      } = Conf,
	    FeatureSubset = rr_example:random_features(Features, NoFeatures),
	    Subset = sample_sane_examples(Examples, Size, Size, Total),
	    #rr_candidate{feature=F} = 
		rr_example:best_split(ExConf, FeatureSubset, Subset, 
				      rr_example:count(Subset), Score, Split, Distribute, Missing),
	    ExSplit = rr_example:split_feature_value(ExConf, F, Examples, Distribute, Missing),
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
    fun (Features, Examples, Total, ExConf, Conf) ->
	    random_subset(Features, Examples, Total, ExConf, Conf, NoFeatures, Variance)
    end.

random_subset(Features, Examples, Total, ExConf, Conf, NoFeatures, Variance) ->
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
    rr_example:best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing).
    

%% @doc
%% resample a decreasing number of features each time an
%% insignificant feature is found, until only one feature is inspected
%% @end
chisquare_decrease(Rate, Sigma) ->
    Sample = redo_curry(subset()),
    redo(Sample, chisquare_decrease_resample(Sample, Rate, Sigma)).
    
chisquare_decrease_resample(_Sample, Rate, Sigma) ->
    fun (#rr_candidate{split=Split}, NoFeatures, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    if Significance < Sigma ->
		    NewNoFeatures = if NoFeatures > 1 ->
					    fun (_) -> round(NoFeatures * Rate) end; 
				       true -> 
					    fun (_) -> 1 end
				    end,
		    NewSample = redo_curry(NewNoFeatures, subset()),
		    {true, NewSample, chisquare_decrease_resample(NewSample, Rate, Sigma)};
	       true ->
		    false
	    end
    end.

%% @doc randomly select either chisquare or subset
random_chisquare(Sigma) ->
    Chi = chisquare(Sigma),
    Sub = subset(),
    random(Chi, Sub, 0.5).

%% @doc resample features if chi-square significance is lower than Sigma
chisquare(Sigma) ->
    Sample = redo_curry(subset()),
    Resample = chisquare_resample(Sigma),
    redo(Sample, Resample).
		
chisquare_resample(Sigma) ->
    fun (#rr_candidate{split=Split}, _, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    if Significance < Sigma ->
		    {true, redo_curry(subset()), chisquare_resample(Sigma)};
	       true ->
		    false
	    end
    end.

%% @doc rame as chisquare-resample, however the resampling are done randomly
randomly_resquare(Factor, Sigma) ->
    Sample = redo_curry(subset()),
    Resample = randomly_resquare_redo(Factor, Sigma),
    redo(Sample, Resample).

randomly_resquare_redo(Factor, Sigma) ->
    fun (#rr_candidate{split=Split}, _, Examples, Total) ->
	    Significance = rr_estimator:chisquare(Split, Examples, Total),
	    Random = random:uniform(),
	    if Significance < Sigma, Random < Factor ->
		    {true, redo_curry(subset()), randomly_resquare_redo(Factor, Sigma)};
	       true ->
		    false
	    end
    end.

%% @doc resample n features m times if gain delta
resample(NoResamples, Delta) ->
    Sample = redo_curry(subset()),
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
weka() ->
    Sample = redo_curry(subset()),
    Resample = weka_resample(),
    redo(Sample, Resample).

weka_resample() ->
    fun (#rr_candidate{score = {Score, _, _}}, _, Examples, Total) ->
	    Gain = (Total * rr_estimator:entropy(Examples)) - Score,
	    if Gain =< 0.0 ->
		    NewSample = redo_curry(fun (_) -> 1 end, subset()),
		    {true, NewSample, weka_resample()};
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
    fun (Features, Examples, Total, ExConf, Conf) ->
	    redo(Features, Examples, Total, ExConf, Conf, 
		 length(Features), #rr_candidate{score = {inf}}, Do, Redo)
    end.

redo([], _, _, _, _, _, _, _, _) ->
    no_information;
redo(Features, Examples, Total, ExConf, Conf, TotalNoFeatures, _Prev, Do, Redo) ->
    {F, {Best, NewFeatures}} = Do(Features, Examples, Total, ExConf, Conf),
    NoFeatures = F(TotalNoFeatures),
    case Redo(Best, NoFeatures, Examples, Total) of
	{true, NewDo, NewRedo} ->
	    redo(ordsets:subtract(Features, ordsets:from_list(NewFeatures)),
		 Examples, Total, ExConf, Conf, 
		 if is_integer(NoFeatures) ->
			 TotalNoFeatures - NoFeatures;
		    true ->
			 TotalNoFeatures
		 end, Best, NewDo, NewRedo); 
	false ->
	    {Best, NewFeatures};
	no_information ->
	    no_information
    end.

%% @doc call either One or Two randomly according to T
random(One, Two, T) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    R = random:uniform(),
	    if R =< T ->
		    One(Features, Examples, Total, ExConf, Conf);
	       true ->
		    Two(Features, Examples, Total, ExConf, Conf)
	    end
    end.

%% @doc apply n Feature selection algorithms and use Choose to pick one
n([],_, Examples, Total, _ExConf, _Conf, Choose, Acc) ->
    Choose(Acc, Examples, Total);
n([Fun|Rest], Features, Examples, Total, ExConf, Conf, Choose, Acc) ->
    n(Rest, Features, Examples, Total, ExConf, Conf, Choose,
      [unpack(Fun(Features, Examples, Total, ExConf, Conf))|Acc]).

%% @doc apply Funs then Choose one
n(Funs, Choose) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    n(Funs, Features, Examples, Total, ExConf,Conf, Choose, [])
    end.

    
%% @doc wrap sample-fun Fun to return the number of sampled features
redo_curry(NoFeatures, Fun) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    {NoFeatures, Fun(Features, Examples, Total, 
			     ExConf, Conf#rf_tree{no_features=NoFeatures})}
    end.

redo_curry(Fun) ->
    fun (Features, Examples, Total, ExConf, Conf) ->
	    {Conf#rf_tree.no_features, Fun(Features, Examples, Total,
					   ExConf, Conf)}
    end.

%% @doc unpack a candidate
-spec unpack(branch()) -> unpacked_branch().
unpack({Candidate, _Features}) ->
    Candidate;
unpack(Candidate) ->
    Candidate.

%% @doc 
%%  oversample NoFeatures * 1+Oversample and calculate their gain, weight a random selection of
%%  NoFeatures from the Oversample population according to their calculated weight
%% @end
oversample(NoFeatures, Oversample) ->
    ok.

%% TODO: remove?
%% depth(NoFeatures) ->
%%     fun (Features, Examples, Total, ExConf, Conf) ->
%% 	    #rf_tree{score = Score, split = Split, distribute = Distribute, 
%% 		     missing_values = Missing, depth = Depth} = Conf,
%% 	    DepthRatio = 1 / (math:log(3+Depth)),
%% 	    NewNoFeatures = case round(NoFeatures * DepthRatio) of
%% 				X when X =< 1 -> 1;
%% 				X when X >= NoFeatures -> NoFeatures;
%% 				X -> X
%% 			    end,
%% 	    NewFeatures = rr_example:random_features(Features, NewNoFeatures),
%% 	    rr_example:best_split(ExConf, NewFeatures, Examples, Total, Score, Split, Distribute, Missing)
%%     end.
				       
%% depth_rule(NoRules, RuleScore) ->
%%     Rule = rule(NoRules, RuleScore),
%%     Subset = subset(),
%%     fun (Features, Examples, Total, ExConf, Conf) ->
%% 	    #rf_tree {
%% 	       depth=Depth,
%% 	       no_features = NoFeatures
%% 	      } = Conf,
%% 	    FeatureCount = NoFeatures(length(Features)),
%% 	    if Depth >= FeatureCount -> 
%% 		    Rule(Features, Examples, Total, ExConf, Conf);
%% 	       true ->
%% 		    Subset(Features, Examples, Total, ExConf, Conf)
%% 	    end
%%     end.
