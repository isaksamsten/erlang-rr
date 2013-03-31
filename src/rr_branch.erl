-module(rr_branch).
-compile(export_all).

-include("rr_tree.hrl").

%%
%% Return a functions which resamples log(Features) + 1 k times if arg
%% max gain(Features) < Delta
%%
resampled(NoResamples, NoFeatures, Delta) ->
    fun (Features, Examples, Total, Conf) ->
	    resampled_subset_branch_split(Features, Examples, Total, Conf, NoResamples, Delta, NoFeatures)
    end.

resampled_subset_branch_split(_Features, _Examples, _Total, 
			      #rr_conf{no_features=NoFeatures}, _, _, _) when NoFeatures =< 0 ->
    no_information;
resampled_subset_branch_split(_Features, _Examples, _Total, _Conf, 0, _, _) ->
    no_information;
resampled_subset_branch_split(Features, Examples, Total, 
			      #rr_conf{no_features=NoFeatures} = Conf, NoResamples, Delta, Log) ->
    Features0 = if NoFeatures =< Log ->
			Features;
		   true ->
			rr_example:random_features(Features, Log)
		end,

    Cand = rr_tree:evaluate_split(Features0, Examples, Total, Conf),
    {Score, _, _} = Cand#rr_candidate.score,
    Gain = (Total*rr_tree:entropy(Examples)) - Score, 
    if  Gain =< Delta ->
	    resampled_subset_branch_split(ordsets:subtract(Features, ordsets:from_list(Features0)), 
					    Examples, Total, Conf#rr_conf{no_features=NoFeatures - Log}, 
					    NoResamples - 1, Delta, Log);
	true ->
	    Cand
    end.

%%
%% Definitly need another way of determine what constitutes a good
%% feature
%%
weighted(NoFeatures, Fraction, NewScores) ->
    fun (_, Examples, Total, Conf) ->
	    weighted_branch_split(NewScores, Examples, Total, Conf, NoFeatures, Fraction)
    end.

weighted_branch_split({Good, _Bad}, Examples, Total, Conf, NoFeatures, _Fraction) ->
    Features0 = rr_example:random_features(Good, NoFeatures),
    rr_tree:evaluate_split(Features0, Examples, Total, Conf).

%%
%% Uses the same algorithm as Weka for resampling non-informative
%% 
weka(NoFeatures) ->
    fun(Features, Examples, Total, Conf) ->
	    weka_branch_split(Features, Examples, Total, Conf, NoFeatures)
    end.

weka_branch_split(_, _, _, #rr_conf{no_features=NoTotal}, _) when NoTotal =< 0 ->
    no_information;
weka_branch_split(Features, Examples, Total, #rr_conf{no_features=NoTotal} = Conf, NoFeatures) ->
    Features0 = if NoTotal =< NoFeatures ->
			Features;
		   true -> 
			rr_example:random_features(Features, NoFeatures)
		end,
    Cand = rr_tree:evaluate_split(Features0, Examples, Total, Conf),
    {Score, _, _} = Cand#rr_candidate.score,
    Gain = (Total*rr_tree:entropy(Examples)) - Score,
    if Gain =< 0.0 ->
	    weka_branch_split(ordsets:subtract(Features, ordsets:from_list(Features0)),
			      Examples, Total, Conf#rr_conf{no_features=NoTotal - NoFeatures}, NoFeatures);
       true ->
	    Cand
    end.

%% 
%% Branch a subset of "NoFeatures" features
%%
subset(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    Features0 = rr_example:random_features(Features, NoFeatures),
	    rr_tree:evaluate_split(Features0, Examples, Total, Conf)
    end.

%%
%% Evaluate the cartesian product of NoFeatures randomly selected
%% attributes
%%
correlation(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    FeaturesA = rr_example:random_features(Features, NoFeatures),
	    FeaturesB = rr_example:random_features(Features, NoFeatures),
	    
	    Combination = [{combined, A, B} || A <- FeaturesA, B <- FeaturesB, A =/= B],

	    %%lists:zipwith(fun (A, B) -> {combined, A, B} end, FeaturesA, FeaturesB),
	    rr_tree:evaluate_split(Combination, Examples, Total, Conf)
    end.

%%
%% Randomly pick either a subset brancher or a correlation brancher
%% according to "Fraction"
%%
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

%%
%% Evalate one randomly selected feature
%%
random(Features, Examples, Total, #rr_conf{no_features=NoFeatures} = Conf) ->
    Feature = lists:nth(random:uniform(NoFeatures), Features),
    rr_tree:evaluate_split([Feature], Examples, Total, Conf).

%%
%% Evaluate all features to find the best split point
%%
all(Features, Examples, Total, Conf) ->
    rr_tree:evaluate_split(Features, Examples, Total, Conf).



rule(NoFeatures) ->
    fun (Features, Examples, Total, Conf) ->
	    rr_rule:best(Features, Examples, Total, Conf, NoFeatures)
    end.

