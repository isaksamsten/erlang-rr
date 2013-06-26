%%% @author Isak Karlsson <isak@Isaks-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for learning rules
%%% @end
%%% Created : 30 Mar 2013 by Isak Karlsson <isak@Isaks-MacBook-Pro.local>
-module(rf_rule).
-author('isak-kar@dsv.su.se').

-export([best/8,
	 distribute_weighted/3,
	 evaluate_rule/3,
	 laplace/2,
	 m_estimate/2,
	 purity/2
	]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @doc generate one best rule
-spec best(features(), examples(), number(), #rr_example{}, Conf::#rf_tree{}, integer(), integer(), score_fun()) -> #rr_candidate{}.
best(Features, Examples, Total, ExConf, Conf, NoFeatures, NoRules, RuleScore) ->
    OneRule = generate_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, RuleScore),
    best_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, NoRules, RuleScore, OneRule).

best_rule(_, _, _, _ExConf, _Conf, _, 0, _, Best) ->
    Best;
best_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, N, RuleScore, Cand) ->
    #rr_candidate{score=Score} = Cand,
    NewCand = generate_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, RuleScore),
    best_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, N - 1, RuleScore,
	      case NewCand#rr_candidate.score < Score of
		  true ->
		      NewCand;
		  false ->
		      Cand
	      end).

%% @private generate a rule predicting a random class
-spec generate_rule(features(), examples(), number(), #rr_example{}, Conf::#rf_tree{}, number(), score_fun()) -> #rr_candidate{}.
generate_rule(Features, Examples, Total, ExConf, Conf, NoFeatures, RuleScore) ->
    #rf_tree{split=Split, score=Score, distribute = Distribute,  missing_values=Missing} = Conf,
    NoClasses = length(Examples),
    {Class, _, _} = lists:nth(random:uniform(NoClasses), Examples),
    Subset = rr_example:random_features(Features, NoFeatures),
    Binary = rr_example:to_binary(Class, Examples),
    Coverage = rr_example:coverage(Binary),
    {Rules, _} = separate_and_conquer(Subset, Binary, Total, ExConf, Conf, RuleScore(Coverage, NoClasses), {[], inf}),
    Rule = {rule, {Rules, Class}, length(Rules)},
    {_Threshold, ExSplit} = Split(ExConf, Rule, Examples, Distribute, Missing),
    #rr_candidate{feature=Rule, score=Score(ExSplit, Total), split=ExSplit}.

%% @doc learn a rule
-spec separate_and_conquer(Features::features(), Examples::examples(), Total::number(), #rr_example{},
			   Conf::#rf_tree{}, score_fun(), {[feature()], number()}) -> [feature()].
separate_and_conquer([], Covered, _, _ExConf, _Conf, _, {Rules, _}) ->
    {Rules, Covered};
separate_and_conquer(Features, Examples, Total, ExConf, Conf, RuleScore, {Rules, Score}) ->
    case learn_one_rule(Features, Examples, Total, RuleScore, ExConf, Conf) of
	1 ->
	    {Rules, Examples};
	{{Feature, _} = Rule, NewScore, Covered}  ->
	    case NewScore < Score of
		true ->
		    case rr_example:coverage(Covered) of
			{Pos, Neg} when Neg =< 0, Pos > 0 ->
			    {[Rule|Rules], Covered};
			{Pos, _} when Pos =< 0 ->
			    {Rules, Covered};
			_NewCoverage ->
			    separate_and_conquer(Features -- [Feature], Covered, Total, ExConf,
						 Conf, RuleScore, {[Rule|Rules], NewScore})
		    end;
		false ->
		    {Rules, Covered}
	    end
    end.

%% @doc learn one additional rule
-spec learn_one_rule(features(), examples(), number(), score_fun(), #rr_example{}, Conf::#rf_tree{}) -> 
			    {feature(), Score::number(), Covered::examples()}.
learn_one_rule(Features, Examples, Total, Score, ExConf,  Conf) ->
    #rf_tree{split = Split, distribute = Distribute, missing_values=Missing} = Conf,
    case rr_example:best_split(ExConf, Features, Examples, Total, Score, Split, Distribute, Missing) of
	no_features ->
	    1;
	#rr_candidate{feature=_Feature, split={_, _AnyExamples}, score={_Score, _, _}} ->
	    1;  % NOTE: {Feature, Score, AnyExamples};
	#rr_candidate{feature=Feature,
		      split={both, LeftExamples, RightExamples},
		      score={_Score,LeftScore, RightScore}}->
	    if LeftScore < RightScore ->
		    {Feature, LeftScore, LeftExamples};
	       true ->
		    {Feature, RightScore, RightExamples}
	    end
    end.
    
%% @doc return a score function for the m-estimate
-spec m_estimate({Pos::number(), Pos::number()}, Total::number()) -> score_fun().
m_estimate(Apriori, M) ->
    fun (Examples, _) ->
	    rr_estimator:m_estimate(Examples, {Apriori, M})
    end.
    
%% @doc return a score function for laplace-corrected purity
-spec laplace({Pos::number(), Pos::number()}, Total::number()) -> score_fun().
laplace(_, Classes) ->
    fun(Examples, _) ->
	    rr_estimator:laplace(Examples, Classes)
    end.

%% @doc return score function for purity measure
-spec purity(None::any(), None::any()) -> score_fun().
purity(_, _) ->
    fun (Examples, _) ->
	rr_estimator:purity(Examples, 0)
    end.


%% @doc distribute examples according to the number of rules fireing for each direction
distribute_weighted(Ex, {rule, Rule, Length}, ExId) ->
    ExCount = rr_example:count(ExId),
    Id = rr_example:exid(ExId),
    case evaluate_weighted_rule(Ex, Rule, ExId, 0, 0, 0) of
	{Left, 0, Missing} ->
	    case Missing of
		0 ->
		    {left, ExCount};
		Missing ->
		    {left, {Id, ExCount * (Left/Length)}, {Id, ExCount * (Missing/Length)}}
	    end;
	{0, Right, Missing} ->
	    case Missing of
		0 ->
		    {right, ExCount};
		Missing ->
		    {right, {Id, ExCount * (Right/Length)}, {Id, ExCount * (Missing/Length)}}
	    end;
	{Left, Right, Missing} ->
	    case Missing of
		0 ->
		    {both, {Id, ExCount * (Left/Length)}, {Id, ExCount*(Right/Length)}};
		Missing ->
		    {all, {Id, ExCount * (Left/Length)}, {Id, ExCount*(Right/Length)}, {Id, ExCount*(Missing/Length)}}
	    end		    
    end;
distribute_weighted(Ex, Feature, ExId) ->
    rr_example:distribute(Ex, Feature, ExId).

%% @private evaluate a rule weighted
evaluate_weighted_rule(Ex, {Rule, _}, ExId, Left, Right, Missing) ->
    evaluate_weighted_rule(Ex, Rule, ExId, Left, Right, Missing);
evaluate_weighted_rule(_, [], _, Left, Right, Missing) ->
    {Left, Right, Missing};
evaluate_weighted_rule(Ex, [Rule|Rest], ExId, NoLeft, NoRight, NoMissing) ->
    case rr_example:distribute(Ex, Rule, ExId) of
	{left, _} ->
	    evaluate_weighted_rule(Ex, Rest, ExId, NoLeft + 1, NoRight, NoMissing);
	{right, _} ->
	    evaluate_weighted_rule(Ex, Rest, ExId, NoLeft, NoRight + 1, NoMissing);
	{'?', _} ->
	    evaluate_weighted_rule(Ex, Rest, ExId, NoLeft, NoRight, NoMissing + 1)
    end.

%% @doc NOTE: if RULE c AND c == true THEN left o/w right
-spec evaluate_rule(#rr_example{}, Rule::rule(), ExId::exid()) -> left | right | '?'.
evaluate_rule(Ex, {Rule, _}, ExId) ->
    evaluate_rule(Ex, Rule, ExId);
evaluate_rule(_, [], _) ->
    left;
evaluate_rule(Ex, [Rule|Rest], ExId) ->
    case rr_example:distribute(Ex, Rule, ExId) of
	{left, _} ->
	    evaluate_rule(Ex, Rest, ExId);
	{right, _} ->
	    right;
	{'?', _} ->
	    '?'
    end.
