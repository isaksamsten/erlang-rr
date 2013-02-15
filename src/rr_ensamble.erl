%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_ensamble).
-compile(export_all).
-include("rr_tree.hrl").


generate_model(Features, Examples, #rr_conf{
				      base_learner = {Classifiers, Base},
				      max_id = MaxId } = Conf) ->
    ets:new(models, [public, named_table]),
    ets:new(predictions, [public, named_table]),
    spawn_base_classifiers(Classifiers, 4, Features, Examples, Base, Conf, MaxId),
    {lists:seq(1, Classifiers), Base}.
    
evaluate_model(Models, Examples, Conf) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Models, Conf, Acc)
		end, dict:new(), Examples).

predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    Prediction = predict_majority(Model, rr_example:example(Example), Conf, []),
    predict_all(Actual, Rest, Model, Conf, dict:update(Actual, fun(Predictions) ->
								 [Prediction|Predictions]
							 end, [Prediction], Dict)).

predict_majority({[], _}, _, _, Acc) ->
    {{C, N}, _Dict} = majority(Acc),
    {C, N};
predict_majority({[N|Rest], Base}, Attr, Conf, Acc) ->
    [{_, Model}|_] = ets:lookup(models, N),
    predict_majority({Rest, Base}, Attr, Conf, [Base:predict(Attr, Model, Conf)|Acc]).

majority(Acc) ->
    Dict = lists:foldl(fun ({Item, _Prob}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1 end, 1, Dict)
		       end, dict:new(), Acc),
    {dict:fold(fun (Class, Count, {MClass, MCount}) ->
		      case Count > MCount of
			  true -> {Class, Count};
			  false -> {MClass, MCount}
		      end
	      end, {undefined, 0}, Dict), Dict}.



spawn_base_classifiers(Sets, Cores, Features, Examples, Base, Conf, MaxId) ->
    Self = self(),
    [spawn_link(fun() ->
			<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
			random:seed({A,B,C}),
			base_generator_process(Self, Base, Conf, MaxId)
		end) || _ <- lists:seq(1, Cores)],
    model_coordinator(Self, Sets, Cores, Features, Examples).

model_coordinator(_, 0, 0, _, _) ->
    done;
model_coordinator(Self, 0, Cores, Features, Examples) ->
    receive
	{more, Self, Pid} ->
	    Pid ! {exit, Self},
	    model_coordinator(Self, 0, Cores, Features, Examples);
	done ->
	    model_coordinator(Self, 0, Cores - 1, Features, Examples)
    end;	    
model_coordinator(Self, Sets, Cores, Features, Examples) ->
    receive 
	{more, Self, Pid} ->
	    Pid ! {batch, Sets, Features, Examples},
	    model_coordinator(Self, Sets - 1, Cores, Features, Examples)
    end.

base_generator_process(Parent, Base, Conf, MaxId) ->
    Parent ! {more, Parent, self()},
    receive
	{batch, Id, Features, Examples} ->
	    {Bag, OutBag} = rr_example:bootstrap_replicate(Examples, MaxId),
	    Model = Base:generate_model(Features, Bag, Conf),
	    Dict = Base:evaluate_model(Model, OutBag, Conf),

	    io:format("Building model ~p (OOB accuracy: ~p) ~n", [Id, rr_eval:accuracy(Dict)]),
	    ets:insert(models, {Id, Model}),
	    base_generator_process(Parent, Base, Conf, MaxId);
	{exit, Parent} ->
	    Parent ! done
    end.
