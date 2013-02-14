%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_eval).


-export([accuracy/1]).

accuracy(Predictions) ->
    {Correct, Incorrect} = correct(Predictions),
    Correct / (Correct + Incorrect).


correct(Predictions) ->
    dict:fold(fun (Actual, Values, Acc) ->
		      lists:foldl(fun({Predict, _},  {C, I}) ->
					  case Actual == Predict of
					      true -> {C+1, I};
					      false -> {C, I+1}
					  end
				  end, Acc, Values)
	      end, {0, 0}, Predictions).
