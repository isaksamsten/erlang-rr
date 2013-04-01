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
    B = best_rule(Features, Examples, Total, Conf, NoFeatures, NoFeatures, OneRule),
    B.

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
    {Class, _, _} = lists:nth(random:uniform(length(Examples)), Examples),
    Subset = rr_example:random_features(Features, NoFeatures),
    Binary = rr_example:to_binary(Class, Examples),
    {Rules, _S, _Cov} = separate_and_conquer(Subset, Binary, Total, Conf, {[], inf}, rr_example:coverage(Binary)),
    Rule = {rule, Rules, length(Rules)},
    {_Threshold, ExSplit} = Split(Rule, Examples, Conf),
%    io:format("~p ~n", [Total]),
    #rr_candidate{feature=Rule,
		  score=Score(ExSplit, Total),
		  split=ExSplit}.

separate_and_conquer([], _Examples, _Total, _Conf, {Rules, Score}, Coverage) ->
    {Rules, Score, Coverage};
separate_and_conquer(Features, Examples, Total, Conf, {Rules, Score}, Coverage) ->
    case learn_one_rule(Features, Examples, Total, Conf, Coverage) of
	1 ->
	    {Rules, 1, Coverage};
	{{Feature, _} = Rule, NewScore, Covered}  ->
	    case NewScore < Score of
		true ->
		    case rr_example:coverage(Covered) of
			{Pos, Neg} when Neg =< 0 ->
			    {[Rule|Rules], NewScore, {Pos, Neg}};
			{Pos, _} when Pos =< 0 ->
			    {Rules, NewScore, Coverage};
			NewCoverage ->
			    separate_and_conquer(Features -- [Feature], Covered, Total, Conf, {[Rule|Rules], NewScore}, NewCoverage)
		    end;
		false ->
		    {Rules, NewScore, Coverage}
	    end
    end.

learn_one_rule(Features, Examples, Total, Conf, _) ->
    case rr_tree:evaluate_split(Features, Examples, Total, Conf) of
	no_features ->
	    1;
	#rr_candidate{split={_, _}} ->
	    1;
	#rr_candidate{feature=Feature,
		      split={both, LeftExamples, RightExamples},
		      score={_Score,LeftScore, RightScore}}->
	    if LeftScore < RightScore ->
		    {Feature, LeftScore, LeftExamples};
	       true ->
		    {Feature, RightScore, RightExamples}
	    end
    end.


%%
laplace_error({both, LeftEx, RightEx}, _Total) ->
    Left = laplace_error(LeftEx),
    Right = laplace_error(RightEx),
    Smallest = if Left > Right ->
		       1 - Left;
		  true ->
		       1 - Right
	       end,
    {Smallest, 1-Left, 1-Right};
laplace_error({left, Side}, _) ->
    Left = 1 - laplace_error(Side),
    {Left, Left, 0.0};
laplace_error({right, Side}, _) ->
    Right = 1 - laplace_error(Side),
    {Right, 0.0, Right}.

laplace_error(Side) ->
    {Pos, Neg} = rr_example:coverage(Side),
    (Pos + 1) / (Pos + Neg + 2).

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

