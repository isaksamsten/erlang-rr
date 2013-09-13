%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for handling sampling approaches
%%% @end
%%% Created : 11 Sep 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_sampling).
-compile(export_all).


oversample_replicate(_) ->
    ok.

smote_replicate(_) ->
    ok.

random(Examples) ->
    Triangle = triangle_variance_sample(0.3, 1, 10, 2),
    Uniform = uniform_variance_sample(0.3, 1, 10),
    case rr_random:uniform() of
	R when R =< 0.33 ->
	    Triangle(Examples);
	R when R >= 0.66 ->
	    Uniform(Examples);
	_ ->
	    bootstrap_replicate(Examples)
    end.
    

triangle_variance_sample(Threshold, A, B, C) ->
    fun (Examples) ->
	    triangle_variance_sample(Examples, Threshold, A, B, C)
    end.

triangle_variance_sample(Examples, Threshold, A, B, C) ->
    select_bootstrap_examples(Examples, fun (_, _) ->
						Include = rr_random:uniform(),
						if Include =< Threshold ->
							error;
						   true ->
							{ok, round(rr_random:triangle(A, B, C))}
						end
					end).

uniform_variance_sample(Threshold, Min, Max) ->
    fun (Examples) ->
	    uniform_variance_sample(Examples, Threshold, Min, Max)
    end.

uniform_variance_sample(Examples, Threshold, Min, Max) ->
    select_bootstrap_examples(Examples, fun (_, _) ->
						Include = rr_random:uniform(),
						if Include =< Threshold ->
							error;
						   true ->
							{ok, round(rr_random:uniform(Min, Max))}
						end
					end).

undersample_replicate(Examples) ->
    Count = rr_example:count(Examples),
    Bootstrap = generate_bootstrap(Count),
    select_bootstrap_examples(Examples, fun (N, Class) ->
						case dict:find(N, Bootstrap) of
						    {ok, Times} ->
							if (Class/Count) > 1/length(Examples) ->
								{ok, Times*(Class/Count)};
							   true ->
								{ok, Times}
							end;
						    error ->
							error
						end
					end).
    
bootstrap_replicate(Examples) ->
    Count = rr_example:count(Examples),
    Bootstrap = generate_bootstrap(Count),
    select_bootstrap_examples(Examples, fun (N, _) ->
						dict:find(N, Bootstrap)
					end).

%% @private
generate_bootstrap(MaxId) ->
    lists:foldl(fun(_, Bootstrap) ->
			dict:update(random:uniform(MaxId), 
				    fun (Count) -> 
					    Count + 1 
				    end, 1, Bootstrap)
		end, dict:new(), lists:seq(1, MaxId)).

select_bootstrap_examples(Examples, Include) ->
    select_bootstrap_examples(Examples, 1, Include, {[], []}).

%% @private
select_bootstrap_examples([], _N, _Include, Acc) ->
    Acc;
select_bootstrap_examples([{Class, Count, Ids}|Examples], N, Include, {InBags, OutBags}) ->
    case select_bootstrap_examples_for_class(Class, {0, 0}, N, Count, Ids, Include, {[], []}) of
	{{_, 0, []}, {_, 0, []}} ->
	    select_bootstrap_examples(Examples, N+Count, Include, {InBags, OutBags});	
	{{_, 0, []}, OutBag} ->
	    select_bootstrap_examples(Examples, N+Count, Include, {InBags, [OutBag|OutBags]});
	{InBag, {_, 0, []}} -> 
	    select_bootstrap_examples(Examples, N+Count, Include, {[InBag|InBags], OutBags});
	{InBag, OutBag} ->
	    select_bootstrap_examples(Examples, N+Count, Include, {[InBag|InBags], [OutBag|OutBags]})
    end.

%% @private
select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, _N, _Total, [], _Include, {InBag, OutBag}) ->
    {{Class, InBagCount, InBag}, 
     {Class, OutBagCount, OutBag}};
select_bootstrap_examples_for_class(Class, {InBagCount, OutBagCount}, N, Total, [ExId|Rest], Include, {InBag, OutBag}) ->
    case Include(N, Total) of
	{ok, Times} ->
	    NewInBag = duplicate_example(ExId, Times, InBag),
	    select_bootstrap_examples_for_class(Class, {InBagCount + Times,  OutBagCount}, N+1, Total,
						Rest, Include, {NewInBag, OutBag});
	error ->
	    select_bootstrap_examples_for_class(Class, {InBagCount,  OutBagCount + 1}, N+1, Total,
						Rest, Include, {InBag, [ExId|OutBag]})
    end.

%% @private
duplicate_example({ExId, _}, N, Acc) ->
    [{rr_example:exid(ExId), N}|Acc];
duplicate_example(ExId, N, Acc) ->
    [{rr_example:exid(ExId), N}|Acc].
