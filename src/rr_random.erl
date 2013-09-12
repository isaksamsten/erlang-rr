%%% @author Isak Karlsson <isak@Unkown-MacBook-Pro.local>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Implementation of some pseudo random distributions
%%% @end
%%% Created : 12 Sep 2013 by Isak Karlsson <isak@Unkown-MacBook-Pro.local>

-module(rr_random).

-compile(export_all).

uniform(X) ->
    random:uniform(X).
uniform() ->
    random:uniform().

%% @doc triangle distributed RV A <= X <= B with mode C
triangle(A, B, C) ->
    U = random:uniform(),
    if 0 < U andalso U < (C-A)/(B-A) ->
	    A + math:sqrt(U * (B-A)*(C-A));
       true ->
	    B - math:sqrt((1-U) * (B-A)*(B-C))
    end.

