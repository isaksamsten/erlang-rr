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

	 squared_chord/2,
	 jensen_difference/2,

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

%% @doc hellinger distance between left and right branch
hellinger(Examples, _Total) ->
    distance(Examples, fun hellinger/1, 
	     fun (Value) -> 
		     1/math:sqrt(2) * math:sqrt(Value) 
	     end).

%% @doc squared-chord difference between left and right branch
squared_chord(Examples, _Total) ->
    distance(Examples, fun squared_chord/1,
	     fun (Value) ->
		     1/2 * Value
	     end).

%% @doc jensen difference between left and right branch
jensen_difference(Examples, _Total) ->
    distance(Examples, fun jensen_difference/1,
	     fun (Value) -> Value end).						

%% @doc generic function for calculating probability distances
distance({both, Left, Right}, FunA, FunB) ->
    Totals = totals(Left ++ Right),
    Value = 1 - FunB(probability_content(Left, Right, Totals, FunA)),
    {Value, Value, Value};
distance({_, _}, _, _) ->
    {1000, 1000, 1000}.
    
%% @todo integrate into distance framework
bhattacharyya({both, Left, Right}, _Total) ->
    case probability_content(Left, Right, [], fun bhattacharyya/1) of
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

totals(Examples) ->
    lists:foldl(fun ({Class, Count, _}, {Classes, Acc}) ->
			{[Class|Classes], dict:update_counter(Class, Count, Acc)}
		end, {[], dict:new()}, Examples).

scale(Examples, Totals) ->
    lists:foldl(fun ({Class, Count, _}, Acc) ->
			dict:store(Class, Count/dict:fetch(Class, Totals), Acc)
		end, dict:new(), Examples).

probability_content(Left, Right, {Classes, Totals} , Fun) ->
    LeftScale = scale(Left, Totals),
    RightScale = scale(Right, Totals),
    LeftTotal = dict:fold(fun (_, V, Acc)  -> V + Acc end, 0, LeftScale),
    RightTotal= dict:fold(fun (_, V, Acc)  -> V + Acc end, 0, RightScale),
    lists:foldl(fun (Class, Count) ->
			P = case dict:find(Class, LeftScale) of {ok, V} -> V/LeftTotal; error -> 0 end,
			Q = case dict:find(Class, RightScale) of {ok, Vq} -> Vq/RightTotal; error -> 0 end,
			Count + Fun({P, Q})
	      end, 0, ordsets:from_list(Classes)).

hellinger({P, Q}) ->
    math:pow(math:sqrt(P) - math:sqrt(Q), 2).

squared_chord({P, Q}) ->
    math:pow(abs(math:sqrt(P) - math:sqrt(Q)), 2).

jensen_difference({P, Q}) ->
    ((P*safe_log(P) + Q * safe_log(Q))/2) - (((P+Q)/2) * safe_log((P+Q)/2)).

safe_log(P) when P =< 0 ->
    0;
safe_log(P) ->
    math:log(P)/math:log(2).

%% @doc calculate information gain (or in reality the entropy
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

mock_splits() ->
    {rr_example:mock_split([{a, 2}, {b, 100001}], [{a, 1}, {b, 99999}]),
     rr_example:mock_split([{b, 100000+99}], [{a, 4}, {b, 100000-99}])}.

hellinger_test() ->
    {Large, Small} = mock_splits(),    
    Distance = fun hellinger/2,
    A = element(1, Distance(Small,8)),
    B = element(1, Distance(Large,8)),
    ?debugFmt("~p", [abs(A-B)]),
    ?assert(A < B).

jensen_test() ->
    {Large, Small} = mock_splits(),    
    Distance = fun jensen_difference/2,
    A = element(1, Distance(Small,8)),
    B = element(1, Distance(Large,8)),
    ?debugFmt("~p", [abs(A-B)]),
    ?assert(A < B).

chord_test() ->
    {Large, Small} = mock_splits(),    
    Distance = fun squared_chord/2,
    A = element(1, Distance(Small,8)),
    B = element(1, Distance(Large,8)),
    ?debugFmt("~p", [abs(A-B)]),
    ?assert(A < B).

unbalance_test() ->
    {Large, Small} = mock_splits(),
    Ai = element(1, info_gain(Small, 100000+99+4+100000-99)),
    Bi = element(1, info_gain(Large, 1000001+2+1+99999)),

    Aj = element(1, hellinger(Small, 100000+99+4+100000-99)),
    Bj = element(1, hellinger(Large, 1000001+2+1+99999)),
    ?debugFmt("~p andalso ~p", [abs(Ai-Bi), abs(Aj-Bj)]),
    ?assert(Ai > Bi andalso Aj < Bj).

-endif.

%% TODO: remove? or use?
%%    Totals = totals(Left ++ Right),
%%     Value = probability_content(Left, Right, Totals, fun hellinger/1),
%%    Hell = 1 - ((1/math:sqrt(2)) * math:sqrt(Value)),
%%     {Hell, Hell, Hell};
%%     Totals = totals(Left ++ Right),
%%     LeftValue = maximize(fun hellinger/1, Left, Totals),
%%     RightValue = maximize(fun hellinger/1, Right, Totals),
%%     Total = 1-((1/math:sqrt(2)) * math:sqrt(LeftValue+RightValue)),
   
%%    {Total, 1-(1/math:sqrt(2))*math:sqrt(LeftValue), 1-(1/math:sqrt(2))*math:sqrt(RightValue)};
%% hellinger({left, _Left}, _) ->
%%    Totals = totals(Left),
%%    LeftValue = 1-((1/2.1)*maximize(fun hellinger/1, Left, Totals)),
%%    {1000, 1000, 1000};
%% hellinger({right, _Right}, _) ->
%%    Totals = totals(Right),
%%    RightValue = 1-((1/2.1)*maximize(fun hellinger/1, Right, Totals)),
%%    {1000, 1000, 1000}.

%%     Totals = totals(Left++Right),
%%     C = 1-((1/2)*probability_content(Left, Right, Totals, fun squared_chord/1)),
%%     {C, C, C}; 
%% %    LeftValue = maximize(fun squared_chord/1, Left, Totals),
%%  %   RightValue = maximize(fun squared_chord/1, Right, Totals),
%%   %  {1-((1/2.1)*(LeftValue+RightValue)), LeftValue, RightValue};
%% squared_chord({left, _Left}, _) ->
%% %    Totals = totals(Left),
%%  %   LeftValue = 1-((1/2.1)*maximize(fun hellinger/1, Left, Totals)),
%%     {1000, 1000, 1000};
%% squared_chord({right, _Right}, _) ->
%%   %  Totals = totals(Right),
%%    % RightValue = 1-((1/2.1)*maximize(fun hellinger/1, Right, Totals)),
%%     {1000, 1000, 1000}.
%% %% squared_chord({left, Left}, _) ->
%% %%     Totals = totals(Left),
%% %%     LeftValue = 1-((1/2.1)*maximize(fun squared_chord/1, Left, Totals)),
%% %%     {LeftValue, LeftValue, 1.0};
%% %% squared_chord({right, Right}, _) ->
%% %%     Totals = totals(Right),
%% %%     RightValue = 1-((1/2.1)*maximize(fun squared_chord/1, Right, Totals)),
%% %%     {RightValue, 1.0, RightValue}.
%% class_total(Examples, Init) ->
%%     lists:foldl(
%%       fun ({Class, Count, _}, {Classes, Counts}) ->
%% 	      {[Class|Classes], Count + Counts}
%%       end, Init, Examples).

%% maximize(Fun, Examples, {Classes, Total}) ->
%%     case ordsets:from_list(lists:map(
%% 	   fun ({X, Y}) ->
%% 		   if X > Y -> {X, Y}; true -> {Y, X} end
%% 	   end,		   
%% 	   [{ClassA, ClassB} || ClassA <- Classes, ClassB <- Classes, ClassA =/= ClassB])) of
%% 	[] ->
%% 	    0.0;
%% 	Pairs ->
%% 	    {Pos0, Neg0} = hd(Pairs),
%% 	    PosFrac0 = rr_example:count(Pos0, Examples)/dict:fetch(Pos0, Total),
%% 	    NegFrac0 = rr_example:count(Neg0, Examples)/dict:fetch(Neg0, Total),
%% 	    lists:foldl(fun ({Pos, Neg}, Max) ->
%% 				PosFrac = rr_example:count(Pos, Examples)/dict:fetch(Pos, Total),
%% 				NegFrac = rr_example:count(Neg, Examples)/dict:fetch(Neg, Total),
%% 				Value = Fun({PosFrac, NegFrac}),
%% 				if Value > Max ->
%% 					Value;
%% 				   true ->
%% 					Max
%% 				end
%% 			end, Fun({PosFrac0, NegFrac0}), tl(Pairs))
%%     end.
