%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Jun 2013 by  <Isak@ISAK-PC>

-module(rr_estimator).
-export([
	 %% decision tree
	 gini/2,
	 info_gain/2,
	 entropy/1,
	 info/2,
	 hellinger/2, 
	 bhattacharyya/2,

	 chord/2,

	 %% rule learner
	 purity/2,
	 laplace/2,
	 m_estimate/2,

	 %% statistical test
	 chisquare/3
	 ]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

%% @doc
gini({both, Left, Right}, Total) ->
    LeftGini = gini_content(Left, Total),
    RightGini = gini_content(Right, Total),
    {1-(LeftGini + RightGini), LeftGini, RightGini};
gini({left, Left}, Total) ->
    LeftGini = gini_content(Left, Total),
    {LeftGini, LeftGini, 0.0};
gini({right, Right}, Total) ->
    RightGini = gini_content(Right, Total),
    {RightGini, 0.0, RightGini}.
    
gini_content(Examples, _Total) -> 
    Counts = [C || {_, C, _} <- Examples],
    Total = lists:sum(Counts),
    lists:foldl(fun (0.0, Count) ->
			Count;
		    (0, Count) ->
			Count;
		    (Class, Count) ->
			Count + math:pow(Class/Total, 2)
		end, 0.0, Counts).


hellinger({both, Left, Right}, _Total) ->
    Value = probability_content(Left, Right, fun hellinger/1),
    Hell = 1 - ((1/math:sqrt(2)) * math:sqrt(Value)),
    {Hell, Hell, Hell};
hellinger({_, _}, _) ->
    {1000, 0.0, 0.0}.

bhattacharyya({both, Left, Right}, _Total) ->
    case probability_content(Left, Right, fun bhattacharyya/1) of
	0.0 ->
	    {1000,0.0,0.0};
	Value ->
	    V = 1-(-1*math:log(Value)),
	    {V, V, V}
    end;
bhattacharyya(_, _) ->
    {1000, 0.0, 0.0}.

bhattacharyya({P, Q}) ->    
    math:sqrt(P*Q).

chord({both, Left, Right}, _Total) ->
    C = 1-(1/2.1*probability_content(Left, Right, fun chord/1)),
    {C, C, C};
chord(_, _) ->
    {1000, 0.0, 0.0}.

class_total(Examples, Init) ->
    lists:foldl(
      fun ({Class, Count, _}, {Classes, Counts}) ->
	      {[Class|Classes], Count + Counts}
      end, Init, Examples).

probability_content(Left, Right, Fun) ->
    {LeftClasses, LeftTotal} = class_total(Left, {[], 0}),
    {Classes, RightTotal} = class_total(Right, {LeftClasses, 0}),
    lists:foldl(fun (Class, Count) ->
			P = rr_example:count(Class, Left)/LeftTotal,
			Q = if RightTotal > 0 -> rr_example:count(Class, Right)/RightTotal;
			       true -> 0
			    end,
			Count + Fun({P, Q})
	      end, 0, ordsets:from_list(Classes)).

hellinger({P, Q}) ->
    math:pow(math:sqrt(P) - math:sqrt(Q), 2).

chord({P, Q}) ->
    math:pow(abs(math:sqrt(P) - math:sqrt(Q)), 1.9).

%% @doc
info_gain({both, Left, Right}, Total) ->
    LeftInfo = info_content(Left, Total),
    RightInfo = info_content(Right, Total),
    {LeftInfo + RightInfo, LeftInfo, RightInfo};
info_gain({left, Left}, Total) ->
    LeftInfo = info_content(Left, Total),
    {LeftInfo, LeftInfo, 0.0};
info_gain({right, Right}, Total) ->
    RightInfo = info_content(Right, Total),
    {RightInfo, 0.0, RightInfo}.

info_content(Side, Total) ->
    NoSide = rr_example:count(Side),
    if NoSide > 0 ->
	    Total * (NoSide / Total) * entropy(Side);
       true ->
	    0.0
    end.

%% @doc calculate the information (weighted by the total)
-spec info(examples(), integer()) -> number().
info(Examples, Total) -> info_content(Examples, Total).
    


%% @doc calculate the entropy
-spec entropy(examples()) -> number().
entropy(Examples) ->
    Counts = [C || {_, C, _} <- Examples],
    entropy(Counts, lists:sum(Counts)).

entropy(Counts, Total) ->
    -1 * lists:foldl(fun (0.0, Count) ->
			     Count;
			 (0, Count) ->
			     Count;
			 (Class, Count) ->
			     Fraction = Class / Total,
			     Count + Fraction * math:log(Fraction)%/math:log(2)
		     end, 0.0, Counts).

m_estimate(Examples, P) ->
    error(Examples, fun m_estimate2/2, P).

laplace(Examples, P) ->
    error(Examples, fun laplace_estimate/2, P).

purity(Examples, P) ->
    error(Examples, fun purity_estimate/2, P).

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


chisquare(Split, Examples, Total) ->
    {Left, Right} = rr_example:unpack_split(Split),
    Pl = rr_example:count(Left)/Total,
    Pr = rr_example:count(Right)/Total,
    NLprim = chisquare_weight(Examples, Pl),
    NRprim = chisquare_weight(Examples, Pr),
    chisquare_sum(Left, NLprim) + chisquare_sum(Right, NRprim).

chisquare_sum(Ns, Nprim) ->
    lists:foldl(fun ({Class, N, _}, Acc) ->
		     case lists:keyfind(Class, 1, Nprim) of
			 false ->
			     Acc + N;
			 {Class, NP} ->
			     Acc + math:pow(N-NP, 2)/NP
		     end
		end, 0.0, Ns).

chisquare_weight(Examples, P) ->
    lists:foldl(fun ({Class, N, _}, Acc) ->
			[{Class, N*P}|Acc]
		end, [], Examples).

-ifdef(TEST).

chi_square_test() ->
    ?assertEqual(chisquare(rr_example:mock_split([{green, 4}, {red, 1}],
						 [{green, 3}, {red, 1}]),
			    rr_example:mock_examples([{green, 7}, {red, 2}]), 
			    9), 
		 0.03214285714285711),
    ?assertEqual(chisquare(rr_example:mock_split([{green, 0}, {red, 8}],
						 [{green, 7}, {red, 2}]),
			    rr_example:mock_examples([{green, 7}, {red, 10}]), 
			    17), 
		 10.577777777777778).

hellinger_test() ->
    Split1 = rr_example:mock_split([{green, 10}, {yellow, 20000}],
				   [{green, 0}, {yellow, 1000}]),
    Split2 = rr_example:mock_split([{green, 10}, {yellow, 20000}],
				   [{green, 8}, {yellow, 1000}]),
    Distance = fun bhattacharyya/2,
    ?debugFmt("~p < ~p", [element(1, Distance(Split1,322)), element(1, Distance(Split2,322))]),
    ?assert(element(1, Distance(Split1, 322)) < element(1, Distance(Split2, 322))).
-endif.
