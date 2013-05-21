%%% @author Isak Karlsson <isak@Isaks-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for learning rules
%%% @end
%%% Created : 30 Mar 2013 by Isak Karlsson <isak@Isaks-MacBook-Pro.local>
-module(rf_rule).
-author('isak-kar@dsv.su.se').

-export([best/7,
	 distribute_weighted/2,
	 evaluate_rule/2,
	 laplace/2,
	 m_estimate/2,
	 purity/2
	]).

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @doc generate one best rule
-spec best(features(), examples(), number(), Conf::#rf_tree{}, integer(), integer(), score_fun()) -> #rr_candidate{}.
best(Features, Examples, Total, Conf, NoFeatures, NoRules, RuleScore) ->
    OneRule = generate_rule(Features, Examples, Total, Conf, NoFeatures, RuleScore),
    best_rule(Features, Examples, Total, Conf, NoFeatures, NoRules, RuleScore, OneRule).

best_rule(_, _, _, _, _, 0, _, Best) ->
    Best;
best_rule(Features, Examples, Total, Conf, NoFeatures, N, RuleScore,  #rr_candidate{score=Score} = Cand) ->
    NewCand = generate_rule(Features, Examples, Total, Conf, NoFeatures, RuleScore),
    best_rule(Features, Examples, Total, Conf, NoFeatures, N - 1, RuleScore,
	      case NewCand#rr_candidate.score < Score of
		  true ->
		      NewCand;
		  false ->
		      Cand
	      end).

%% @private generate a rule predicting a random class
-spec generate_rule(features(), examples(), number(), Conf::#rf_tree{}, number(), score_fun()) -> #rr_candidate{}.
generate_rule(Features, Examples, Total, Conf, NoFeatures, RuleScore) ->
    #rf_tree{split=Split, score=Score, distribute = Distribute,  missing_values=Missing} = Conf,
    NoClasses = length(Examples),
    {Class, _, _} = lists:nth(random:uniform(NoClasses), Examples),
    Subset = rr_example:random_features(Features, NoFeatures),
    Binary = rr_example:to_binary(Class, Examples),
    Coverage = rr_example:coverage(Binary),
    {Rules, _} = separate_and_conquer(Subset, Binary, Total, Conf, RuleScore(Coverage, NoClasses), {[], inf}),
    Rule = {rule, {Rules, Class}, length(Rules)},
    {_Threshold, ExSplit} = Split(Rule, Examples, Distribute, Missing),
    #rr_candidate{feature=Rule, score=Score(ExSplit, Total), split=ExSplit}.

%% @doc learn a rule
-spec separate_and_conquer(Features::features(), Examples::examples(), Total::number(),
			   Conf::#rf_tree{}, score_fun(), {[feature()], number()}) -> [feature()].
separate_and_conquer([], Covered, _, _, _, {Rules, _}) ->
    {Rules, Covered};
separate_and_conquer(Features, Examples, Total, Conf, RuleScore, {Rules, Score}) ->
    case learn_one_rule(Features, Examples, Total, RuleScore, Conf) of
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
			    separate_and_conquer(Features -- [Feature], Covered, Total, Conf, RuleScore, {[Rule|Rules], NewScore})
		    end;
		false ->
		    {Rules, Covered}
	    end
    end.

%% @doc learn one additional rule
-spec learn_one_rule(features(), examples(), number(), score_fun(), Conf::#rf_tree{}) -> {feature(), Score::number(), Covered::examples()}.
learn_one_rule(Features, Examples, Total, Score, #rf_tree{split = Split, distribute = Distribute, missing_values=Missing}) ->
    case rr_example:best_split(Features, Examples, Total, Score, Split, Distribute, Missing) of
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
	    error(Examples, fun m_estimate2/2, {Apriori, M})
    end.
    
%% @doc return a score function for laplace-corrected purity
-spec laplace({Pos::number(), Pos::number()}, Total::number()) -> score_fun().
laplace(_, Classes) ->
    fun(Examples, _) ->
	    error(Examples, fun laplace_estimate/2, Classes)
    end.

%% @doc return score function for purity measure
-spec purity(None::any(), None::any()) -> score_fun().
purity(_, _) ->
    fun (Examples, _) ->
	    error(Examples, fun purity_estimate/2, 0)
    end.

%% @private calculate the purity
purity_estimate(Side, 0) ->
    {P, N} = rr_example:coverage(Side),
    if P > 0 -> P / (P+N); true -> 0.0 end.

%% @private calculate m estimated 
m_estimate2(Side, {{Pos, Neg}, M}) ->
    {P, N} = rr_example:coverage(Side),
    Pi = if Pos > 0 -> Pos / (Pos + Neg); true -> 0.0 end,
    (P+M*Pi)/(P+N+M).

%% @private calculate laplace corrected purity
laplace_estimate(Side, Classes) ->
    {Pos, Neg} = rr_example:coverage(Side),
    (Pos + 1) / (Pos + Neg + Classes).

-spec error(Split::split(), fun((examples(), any()) -> number()), any()) -> score().
error({both, LeftEx, RightEx}, Fun, Payload) ->
    Left = 1 - Fun(LeftEx, Payload),
    Right = 1 - Fun(RightEx, Payload),
    Smallest = if Left < Right ->
		       Left;
		  true ->
		       Right
	       end,
    {Smallest, Left, Right};
error({left, Side}, Fun, Payload) ->
    Left = 1 - Fun(Side, Payload),
    {Left, Left, 1.0};
error({right, Side}, Fun, Payload) ->
    Right = 1 - Fun(Side, Payload),
    {Right, 1.0, Right}.

%% @doc distribute examples according to the number of rules fireing for each direction
distribute_weighted({rule, Rule, Length}, ExId) ->
    ExCount = rr_example:count(ExId),
    Id = rr_example:exid(ExId),
    case evaluate_weighted_rule(Rule, ExId, 0, 0, 0) of
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
distribute_weighted(Feature, ExId) ->
    rr_example:distribute(Feature, ExId).

%% @private evaluate a rule weighted
evaluate_weighted_rule({Rule, _}, ExId, Left, Right, Missing) ->
    evaluate_weighted_rule(Rule, ExId, Left, Right, Missing);
evaluate_weighted_rule([], _, Left, Right, Missing) ->
    {Left, Right, Missing};
evaluate_weighted_rule([Rule|Rest], ExId, NoLeft, NoRight, NoMissing) ->
    case rr_example:distribute(Rule, ExId) of
	{left, _} ->
	    evaluate_weighted_rule(Rest, ExId, NoLeft + 1, NoRight, NoMissing);
	{right, _} ->
	    evaluate_weighted_rule(Rest, ExId, NoLeft, NoRight + 1, NoMissing);
	{'?', _} ->
	    evaluate_weighted_rule(Rest, ExId, NoLeft, NoRight, NoMissing + 1)
    end.

%% @doc NOTE: if RULE c AND c == true THEN left o/w right
-spec evaluate_rule(Rule::rule(), ExId::exid()) -> left | right | '?'.
evaluate_rule({Rule, _}, ExId) ->
    evaluate_rule(Rule, ExId);
evaluate_rule([], _) ->
    left;
evaluate_rule([Rule|Rest], ExId) ->
    case rr_example:distribute(Rule, ExId) of
	{left, _} ->
	    evaluate_rule(Rest, ExId);
	{right, _} ->
	    right;
	{'?', _} ->
	    '?'
    end.
