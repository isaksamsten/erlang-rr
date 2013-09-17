%%% @author Isak Karlsson <isak@Isaks-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created :  7 May 2013 by Isak Karlsson <isak@Isaks-MacBook-Pro.local>

-module(rr_rex).

%% @headerfile "rex.hrl"
-include("rex.hrl").

-export([
	 extract/4
	]).

extract({_, Model, _}, Confidence, Coverage, Total) ->
    extract(Model, Confidence, Coverage, Total, [], []).

extract(#rf_leaf{distribution={True, False}, class=Class}, MinConfidence, MinCoverage, Total, Rule, Acc) ->
    Confidence = confidence(True, False),
    Coverage = coverage(True, False, Total),
    if Coverage =< MinCoverage;
       Confidence =< MinConfidence ->
	    Acc;
       true ->
	    [{rule, lists:reverse(Rule), Confidence, Coverage, Class}|Acc]
    end;
extract(#rf_node{feature=Feature, 
		 left=Left, 
		 right=Right, 
		 distribution={LeftCount, RightCount, {Class, True}}}, MinConfidence, MinCoverage, Total, Rule, Acc) ->
    False = (LeftCount+RightCount)-True,
    
    Confidence = confidence(True, False),
    Coverage = coverage(True, False, Total),
    if Coverage >= MinCoverage ->
	    LeftFeature = [{left, Feature}|Rule], %% lt or eq
	    RightFeature = [{right, Feature}|Rule], %% gt or not eq
	    if Confidence >= MinConfidence ->
		    LeftAcc = extract(Left, MinConfidence, MinCoverage, Total, LeftFeature, Acc),
		    NewAcc = case Rule of
				 [] -> LeftAcc;
				 _ ->  [{rule, lists:reverse(Rule), Confidence, Coverage, Class}|LeftAcc]
			     end,
		    extract(Right, MinConfidence, MinCoverage, Total, RightFeature, NewAcc);
	       true -> 
		    LeftAcc = extract(Left, MinConfidence, MinCoverage, Total, LeftFeature, Acc),
		    extract(Right, MinConfidence, MinCoverage, Total, RightFeature, LeftAcc)
	    end;
	    
       true ->
	    Acc
    end.

confidence(True, False) ->
    True/(True+False).
coverage(True, False, Total) ->
    (True+False)/Total.
