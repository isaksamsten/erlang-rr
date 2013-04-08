%%% @author Isak Karlsson <isak@Isaks-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 30 Mar 2013 by Isak Karlsson <isak@Isaks-MacBook-Pro.local>

-module(rr_rule).
-compile(export_all).

-include("rr_tree.hrl").

best(Features, Examples, Total, Conf, NoFeatures) ->
    OneRule = generate_rule(Features, Examples, Total, Conf, NoFeatures),
    best_rule(Features, Examples, Total, Conf, NoFeatures, NoFeatures, OneRule).

best_rule(_, _, _, _, _, 0, Best) ->
    Best;
best_rule(Features, Examples, Total, Conf, NoFeatures, N, #rr_candidate{score=Score} = Cand) ->
    NewCand = generate_rule(Features, Examples, Total, Conf, NoFeatures),
    best_rule(Features, Examples, Total, Conf, NoFeatures, N - 1, 
	      case NewCand#rr_candidate.score < Score of
		  true ->
		      NewCand;
		  false ->
		      Cand
	      end).

generate_rule(Features, Examples, Total, #rr_conf{split=Split, score=Score} = Conf, NoFeatures) ->
    NoClasses = length(Examples),
    {Class, _, _} = lists:nth(random:uniform(NoClasses), Examples),
    Subset = rr_example:random_features(Features, NoFeatures),
    Binary = rr_example:to_binary(Class, Examples),
    Coverage = rr_example:coverage(Binary),
    Rules = separate_and_conquer(Subset, Binary, Total, Conf#rr_conf{score=laplace_error(NoClasses)}, 
					     {[], inf}, rr_example:coverage(Binary)),
    Rule = {rule, Rules, length(Rules)},
    {_Threshold, ExSplit} = Split(Rule, Examples, Conf),
    #rr_candidate{feature=Rule,
		  score=Score(ExSplit, Total),
		  split=ExSplit}.

separate_and_conquer([], _, _, _, {Rules, _}, _) ->
    Rules;
separate_and_conquer(Features, Examples, Total, Conf, {Rules, Score}, Coverage) ->
    case learn_one_rule(Features, Examples, Total, Conf, Coverage) of
	1 ->
	    Rules;
	{{Feature, _} = Rule, NewScore, Covered}  ->
	    case NewScore < Score of
		true ->
		    case rr_example:coverage(Covered) of
			{Pos, Neg} when Neg =< 0, Pos > 0 ->
			    [Rule|Rules];
			{Pos, _} when Pos =< 0 ->
			    Rules;
			NewCoverage ->
			    separate_and_conquer(Features -- [Feature], Covered, Total, Conf, {[Rule|Rules], NewScore}, NewCoverage)
		    end;
		false ->
		    Rules
	    end
    end.

learn_one_rule(Features, Examples, Total, Conf, _) ->
    case rr_tree:evaluate_split(Features, Examples, Total, Conf) of
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


m_estimate(Apriori, M) ->
    fun (Examples, _) ->
	    m_estimated_error(Examples, Apriori, M)
    end.

m_estimated_error({both, LeftEx, RightEx}, A, M) ->
    Left = 1 - m_estimate(LeftEx, A, M),
    Right = 1 - m_estimate(RightEx, A, M),
    Smallest = if Left < Right ->
		       Left;
		  true ->
		       Right
	       end,
    {Smallest, Left, Right};
m_estimated_error({left, LeftEx}, A, M) ->
    Left = 1 - m_estimate(LeftEx, A, M),
    {Left, Left, 1.0};
m_estimated_error({right, RightEx}, A, M) ->
    Right = 1 - m_estimate(RightEx, A, M),
    {Right, 1.0, Right}.

m_estimate(Side, {Pos, Neg}, M) ->
    {P, N} = rr_example:coverage(Side),
    Pi = if Pos > 0 -> Pos / (Pos + Neg); true -> 0.0 end,
    (P+M*Pi)/(P+N+M).
    

laplace_error(Classes) ->
    fun(Examples, _) ->
	    laplace_error(Examples, Classes)
    end.

laplace_error({both, LeftEx, RightEx}, Classes) ->
    Left = 1 - laplace(LeftEx, Classes),
    Right = 1 - laplace(RightEx, Classes),
    Smallest = if Left < Right ->
		       Left;
		  true ->
		       Right
	       end,
    {Smallest, Left, Right};
laplace_error({left, Side}, Classes) ->
    Left = 1 - laplace(Side, Classes),
    {Left, Left, 1.0};
laplace_error({right, Side}, Classes) ->
    Right = 1 - laplace(Side, Classes),
    {Right, 1.0, Right}.

laplace(Side, Classes) ->
    {Pos, Neg} = rr_example:coverage(Side),
    (Pos + 1) / (Pos + Neg + Classes).



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

%% NOTE: if RULE c AND c == true THEN left o/w right
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

