%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_ensamble).
-compile(export_all).
-include("rr_tree.hrl").

%%
%% Generate an ensamble of models from of #rr_conf.base_learners
%%
generate_model(Features, Examples, #rr_conf{
				      base_learner = {Classifiers, Base},
				      max_id = MaxId,
				      cores = Cores} = Conf) ->
    ets:new(predictions, [public, named_table]),
    spawn_base_classifiers(Classifiers, Cores, Features, Examples, Base, Conf, MaxId).
    

%%
%% Evaluate "Model" on "Examples"
%%  Models: Pid to 'evaluator_coordinator'
%%    
evaluate_model(Models, Examples, Conf) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Models, Conf, Acc)
		end, dict:new(), Examples).

predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    Prediction = predict_majority(Model, Example, Conf),
    predict_all(Actual, Rest, Model, Conf, dict:update(Actual, fun(Predictions) ->
								 [Prediction|Predictions]
							 end, [Prediction], Dict)).
%%
%% Predict 
%%
predict_majority(Model, Example, #rr_conf{base_learner={N, _}}) ->
    Model ! {evaluate, self(), Example},
    receive
	{prediction, Model, Predictions} ->
	    majority(Predictions, N)
    end.

majority(Acc, N) ->
    Dict = lists:foldl(fun ({Item, _Laplace}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1  end, 1, Dict)
		       end, dict:new(), Acc),
    dict:fold(fun (Class, Count, {MClass, MCount}) ->
		      case Count > MCount of
			  true -> {Class, Count};
			  false -> {MClass, MCount}
		      end
	      end, {undefined, 0}, Dict).

spawn_base_classifiers(Sets, Cores, Features, Examples, Base, Conf, MaxId) ->
    Self = self(),
    Coordinator = spawn_link(fun() -> build_coordinator(Self, Sets, Cores, Features, Examples) end),
    [spawn_link(fun() -> base_build_process(Coordinator, Base, Conf, MaxId) end) 
		 || _ <- lists:seq(1, Cores)],
    Coordinator.
    

collect_predictions([], _, Acc) ->
    Acc;    
collect_predictions(Processes, Coordinator, Acc) ->
    receive
	{prediction, Coordinator, Pid, Predictions} ->
	    collect_predictions(Processes -- [Pid], Coordinator, Acc ++ Predictions)
    end.

submit_prediction(Processes, Coordinator, ExId) ->
    lists:foreach(fun(Process) -> Process ! {evaluate, Coordinator, ExId} end, Processes),
    collect_predictions(Processes, Coordinator, []).
			  

evaluation_coordinator(Parent, Coordinator, Processes) ->
    receive 
	{evaluate, Parent, ExId} ->
	    Prediction = submit_prediction(Processes, Coordinator, ExId),
	    Parent ! {prediction, Coordinator, Prediction},
	    evaluation_coordinator(Parent, Coordinator, Processes);
	{exit, Parent} ->
	    done
    end.

transition_coordinator(Parent, Coordinator, 0, Acc) ->
    evaluation_coordinator(Parent, Coordinator, Acc);
transition_coordinator(Parent, Coordinator, Cores, Acc) ->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid|Acc])
    end.

build_coordinator(Parent, Sets, Cores, Features, Examples) ->
    build_coordinator(Parent, self(), 1, Sets, Cores, Features, Examples).

build_coordinator(Parent, Coordinator, Counter, Sets, Cores, _Features, _Examples) when Sets < Counter->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid])
    end;
build_coordinator(Parent, Coordinator, Counter, Sets, Cores, Features, Examples) ->
    receive 
	{build, Coordinator, Pid} ->
	    Pid ! {build, Counter, Features, Examples},
	    build_coordinator(Parent, Coordinator, Counter + 1, Sets, Cores, Features, Examples)
    end.

base_build_process(Coordinator, Base, Conf, MaxId) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    base_build_process(Coordinator, Base, Conf, MaxId, []).

base_build_process(Coordinator, Base, Conf, MaxId, Acc) ->
    Coordinator ! {build, Coordinator, self()},
    receive
	{build, Id, Features, Examples} ->
	    {Bag, OutBag} = rr_example:bootstrap_replicate(Examples, MaxId),
	    Conf0 = Conf#rr_conf{evaluate = case Conf#rr_conf.evaluate of
						{random, Prob} -> random_evaluator(Prob);
						Fun -> Fun
					    end},
	    Model = Base:generate_model(Features, Bag, Conf0),
	    Dict = Base:evaluate_model(Model, OutBag, Conf0),

	    io:format("Building model ~p (OOB accuracy: ~p) ~n", [Id, rr_eval:accuracy(Dict)]),
	    base_build_process(Coordinator, Base, Conf, MaxId, [Model|Acc]);
	{completed, Coordinator} ->
	    base_evaluator_process(Coordinator, Base, Conf, Acc)
    end.

base_evaluator_process(Coordinator, Base, Conf, Models)->
    receive
	{evaluate, Coordinator, ExId} ->
	    Coordinator ! {prediction, Coordinator, self(), make_prediction(Models, Base, ExId, Conf)},
	    base_evaluator_process(Coordinator, Base, Conf, Models);
	{exit, Coordinator} ->
	    done
    end.

%%
%% Use models built using "Base" to predict the class of "ExId"
%%
make_prediction(Models, Base, ExId, Conf) ->
    make_prediction(Models, Base, ExId, Conf, []).

make_prediction([], _Base, _ExId, _Conf, Acc) ->
    Acc;
make_prediction([Model|Models], Base, ExId, Conf, Acc) ->
    make_prediction(Models, Base, ExId, Conf,
		    [Base:predict(rr_example:example(ExId), Model, Conf)|Acc]).

random_evaluator(Prob) ->
    rr_tree:random_evaluator(random:uniform() * Prob).

