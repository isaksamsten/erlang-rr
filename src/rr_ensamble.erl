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
				      cores = Cores} = Conf) ->
%    ets:new(predictions, [public, named_table]),
    spawn_base_classifiers(Classifiers, Cores, Features, Examples, Base, Conf).
    

%%
%% Evaluate "Model" on "Examples"
%%  Models: Pid to 'evaluator_coordinator'
%%    
evaluate_model(Models, Examples, Conf) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Models, Conf, Acc)
		end, dict:new(), Examples). %% TODO: Model ! {exit, self()}

predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    {Prediction, Probs} = predict_majority(Model, Example, Conf),
    predict_all(Actual, Rest, Model, Conf, dict:update(Actual, fun(Predictions) ->
								 [{Prediction, Probs}|Predictions]
							 end, [{Prediction, Probs}], Dict)).
%%
%% Predict 
%%
predict_majority(Model, Example, #rr_conf{base_learner={N, _}}) ->
    Model ! {evaluate, self(), Example},
    receive
	{prediction, Model, Predictions} ->
	    Probs = get_prediction_probabilities(Predictions, N),
	    {hd(Probs), Probs}
    end.

%%
%% Get the predicted probabilites (i.e. the number of votes/total
%% number of models)
%%
get_prediction_probabilities(Acc, N) ->
    Dict = lists:foldl(fun ({Item, _Laplace}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1  end, 1, Dict)
		       end, dict:new(), Acc),
    lists:sort(fun({_, Ca}, {_, Cb}) -> Ca > Cb end, 
	       lists:foldl(fun ({Class, Count}, Probs) ->
				   [{Class, Count/N}|Probs]
			   end, [], dict:to_list(Dict))).

spawn_base_classifiers(Sets, Cores, Features, Examples, Base, Conf) ->
    Self = self(),
    Coordinator = spawn_link(fun() -> build_coordinator(Self, Sets, Cores, Features, Examples) end),
    [spawn_link(fun() -> base_build_process(Coordinator, Base, Conf) end) 
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
    lists:foreach(fun(Process) -> Process ! {evaluate, Coordinator, Process, ExId} end, Processes),
    collect_predictions(Processes, Coordinator, []).
			  

%%
%% Coordinate the processes responsible for building the models, and
%% now let them evaluate examples
%%
evaluation_coordinator(Parent, Coordinator, Processes) ->
    receive 
	{evaluate, Parent, ExId} ->
	    Prediction = submit_prediction(Processes, Coordinator, ExId),
	    Parent ! {prediction, Coordinator, Prediction},
	    evaluation_coordinator(Parent, Coordinator, Processes);
	{exit, Parent} ->
	    done %% TODO: lists:foreach(fun(Process) -> Process ! {exit, Coordinator} end, Processes)
    end.

%%
%% Transition every build process into an evaluator process
%%
transition_coordinator(Parent, Coordinator, 0, Acc) ->
    io:format(standard_error, "~n", []), % Note: separate progress (sorry)
    evaluation_coordinator(Parent, Coordinator, Acc);
transition_coordinator(Parent, Coordinator, Cores, Acc) ->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid|Acc])
    end.

%%
%% coordinates the build process by sending them a notification (and
%% an id) for building the next tree
%%
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

%%
%% Process for building bootap replicas and train "Base". Transitions
%% into 'base_evaluator_process', at {completed, Coordinator}
%%
base_build_process(Coordinator, Base, Conf) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    base_build_process(Coordinator, Base, Conf, []).

base_build_process(Coordinator, Base, #rr_conf{base_learner={T,_},
					       progress=Progress,
					       evaluate=Evaluate,
					       score=Score} = Conf, Acc) ->
    Coordinator ! {build, Coordinator, self()},
    receive
	{build, Id, Features, Examples} ->
	    {Bag, OutBag} = rr_example:bootstrap_replicate(Examples),
	    Conf0 = Conf#rr_conf{evaluate = case Evaluate of
						{random, Prob} -> random_evaluator(Prob);
						Fun -> Fun
					    end},
	    Conf1 = Conf0#rr_conf{score = case Score of
					     ranpdom ->
						 random_score();
					     Fun0 -> Fun0
					 end},
	    Model = Base:generate_model(Features, Bag, Conf1),
	    Rem = if T > 10 -> round(T/10); true -> 1 end,
	    case Id rem Rem of
		0 ->
		    Progress(Id, T);
		_ ->
		    ok
	    end,
	    base_build_process(Coordinator, Base, Conf, [Model|Acc]);
	{completed, Coordinator} ->
	    base_evaluator_process(Coordinator, self(), Base, Conf, Acc)
    end.

%%
%% Use a random score function (i.e. either 
%%
random_score() ->
    Random = random:uniform(),
    if Random >= 0.5 ->
	    fun rr_tree:info/2;
       true ->
	    fun rr_tree:gini/2
    end.
	    
%%
%% Recives, {evaluate, Coordinator, ExId}, where "ExId" is an
%% example. The correct class for "ExId" is predicted using "Models"
%%
base_evaluator_process(Coordinator, Self, Base, Conf, Models)->
    receive
	{evaluate, Coordinator, Self, ExId} ->
	    Coordinator ! {prediction, Coordinator, Self, make_prediction(Models, Base, ExId, Conf)},
	    base_evaluator_process(Coordinator, Self, Base, Conf, Models);
	{exit, Coordinator, Self} ->
	    done
    end.

%%
%% Use models built using "Base" to predict the class of "ExId"
%%
make_prediction(Models, Base, ExId, Conf) ->
    make_prediction(Models, Base, ExId, Conf, []).

make_prediction([], _Base, _ExId, _Conf, Acc) ->
    Acc;
make_prediction([Model|Models], Base, ExId, #rr_conf{log=Log} = Conf, Acc) ->
    {Prediction, NodeNr} = Base:predict(ExId, Model, []),
    Log(debug, "Example: ~p predicted (~p) by: ~w", [ExId, Prediction, NodeNr]),
    make_prediction(Models, Base, ExId, Conf, [Prediction|Acc]).

%%
%% Returns a random evaluator from 'rr_tree' with Probability 0 < p <=
%% Prob
%%
random_evaluator(Prob) ->
    rr_tree:random_evaluator(random:uniform() * Prob).

