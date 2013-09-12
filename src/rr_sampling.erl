%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for handling sampling approaches
%%% @end
%%% Created : 11 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_sampling).
-compile(export_all).


undersample_replicate(_) ->
    ok.

oversample_replicate(_) ->
    ok.

smote_replicate(_) ->
    ok.

highvariance_sample(Threshold, A, B, C) ->
    fun (Examples) ->
	    highvariance_sample(Examples, Threshold, A, B, C)
    end.

highvariance_sample(Examples, Threshold, A, B, C) ->
    select_bootstrap_examples(Examples, 1, [],
			      fun (_, _) ->
				      Include = rr_random:uniform(),
				      if Include =< Threshold ->
					      error;
					 true ->
					      {ok, round(rr_random:triangle(A, B, C))}
				      end
			      end).

bootstrap_replicate(Examples) ->
    Bootstrap = generate_bootstrap(rr_example:count(Examples)),
    select_bootstrap_examples(Examples, 1, Bootstrap, fun dict:find/2).

%% @private
generate_bootstrap(MaxId) ->
    lists:foldl(fun(_, Bootstrap) ->
			dict:update(random:uniform(MaxId), fun (Count) -> Count + 1 end, 1, Bootstrap)
		end, dict:new(), lists:seq(1, MaxId)).


select_bootstrap_examples(Examples, N, Bootstrap, Include) ->
    select_bootstrap_examples(Examples, N, Bootstrap, Include, {[], []}).

%% @private
select_bootstrap_examples([], _N, _Bootstrap, _Include, Acc) ->
    Acc;
select_bootstrap_examples([{Class, Count, Ids}|Examples], N, Bootstrap, Include, {InBags, OutBags}) ->
    case select_bootstrap_examples_for_class(Class, {0, 0}, N, Ids, Bootstrap, Include, {[], []}) of
	{{_, 0, []}, {_, 0, []}} ->
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, Include, {InBags, OutBags});	
	{{_, 0, []}, OutBag} ->
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, Include, {InBags, [OutBag|OutBags]});
	{InBag, {_, 0, []}} -> 
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, Include, {[InBag|InBags], OutBags});
	{InBag, OutBag} ->
	    select_bootstrap_examples(Examples, N+Count, Bootstrap, Include, {[InBag|InBags], [OutBag|OutBags]})
    end.

%% @private
select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, _N, [], 
				    _Boostrap, _Include, {InBag, OutBag}) ->
    {{Class, InBagCount, InBag}, 
     {Class, OutBagCount, OutBag}};
select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, N, [ExId|Rest], 
				    Bootstrap, Include, {InBag, OutBag}) ->
    case Include(N, Bootstrap) of
	{ok, Times} ->
	    NewInBag = duplicate_example(ExId, Times, InBag),
	    select_bootstrap_examples_for_class(Class, {InBagCount + Times,  OutBagCount},
						N+1, Rest, Bootstrap, Include, {NewInBag, OutBag});
	error ->
	    select_bootstrap_examples_for_class(Class, {InBagCount,  OutBagCount + 1},
						N+1, Rest, Bootstrap, Include, {InBag, [ExId|OutBag]})
    end.

%% @private
duplicate_example({ExId, _}, N, Acc) ->
    [{rr_example:exid(ExId), N}|Acc];
duplicate_example(ExId, N, Acc) ->
    [{rr_example:exid(ExId), N}|Acc].
