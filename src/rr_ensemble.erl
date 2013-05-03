%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_ensemble).
-author('isak-kar@dsv.su.se').
-compile(export_all). %% TODO: export

%% @headerfile "rr_tree.hrl"
-include("rr_tree.hrl").

init() ->
    ets:new(models, [public, named_table]).

save_model(Model, File) ->
    Model ! {exit, self()},
    receive
	{done, Model} ->
	    ets:tab2file(models, File)
    end.

load_model(File) ->
    ets:file2tab(File),
    load_model().

load_model() ->
    throw({error, not_implemented}).

%%
%% Generate an ensamble of models from of #rr_conf.base_learners
%%
generate_model(Features, Examples, #rr_conf{
				      base_learner = {Classifiers, Base},
				      cores = Cores} = Conf) ->
    Model = spawn_base_classifiers(Classifiers, Cores, Features, Examples, Base, Conf),
    receive
	{done, Model} ->
	    Model
    end.
    

%%
%% Evaluate "Model" on "Examples"
%%  Models: Pid to 'evaluator_coordinator'
%%    
evaluate_model(Models, Examples, Conf) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Models, Conf, Acc)
		end, dict:new(), Examples). %% TODO: Model ! {exit, self()}

variable_importance(Model, #rr_conf{base_learner={Classifiers, _}}) ->
    Model ! {importance, self()},
    receive
	{importance, Model, Importance} ->
	    update_variable_importance(Importance, dict:new(), Classifiers)
    end.
	    
%% @todo fix this	
qeury(Model, Conf, {Method, TreeFun, CollectFun}) ->
    Model ! {Method, TreeFun, self()},
    receive
	{Method, Model, Return} ->
	    CollectFun(Return, Conf)
    end.

predict_all(_, [], _, _, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, Conf, Dict) ->
    {Prediction, Probs} = predict_majority(Model, Example, Conf),
    rr_example:insert_prediction(Example, Probs),
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
    Dict = lists:foldl(fun ({{Item, _Laplace}, _NodeNr}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1  end, 1, Dict)
		       end, dict:new(), Acc),
    lists:sort(fun({_, Ca}, {_, Cb}) -> Ca > Cb end, 
	       lists:foldl(fun ({Class, Count}, Probs) ->
				   [{Class, Count/N}|Probs]
			   end, [], dict:to_list(Dict))).

%%
%% Spaws classification and evaluator process
%%
spawn_base_classifiers(Sets, Cores, Features, Examples, Base, Conf) ->
    Self = self(),
    Coordinator = spawn_link(fun() -> build_coordinator(Self, Sets, Cores) end),
    [spawn_link(fun() -> base_build_process(Coordinator, Base, Features, Examples, Conf) end) 
		 || _ <- lists:seq(1, Cores)],
    Coordinator.
    

collect_task(_, [], _, Acc) ->
    Acc;    
collect_task(Task, Processes, Coordinator, Acc) ->
    receive
	{Task, Coordinator, Pid, Predictions} ->
	    collect_task(Task, Processes -- [Pid], Coordinator, Acc ++ Predictions)
    end.

submit_prediction(Processes, Coordinator, ExId) ->
    lists:foreach(fun(Process) -> Process ! {evaluate, Coordinator, Process, ExId} end, Processes),
    collect_task(prediction, Processes, Coordinator, []).

submit_importance(Processes, Coordinator) ->
    lists:foreach(fun(Process) -> Process ! {importance, Coordinator, Process} end, Processes),
    collect_task(importance, Processes, Coordinator, []).

submit_query(Method, TreeFun, Processes, Coordinator) ->
    lists:foreach(fun(Process) -> Process ! {Method, TreeFun, Coordinator, Process} end, Processes),
    collect_task(Method, Processes, Coordinator, []).
			   
			  

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
	{importance, Parent} ->
	    Importance = submit_importance(Processes, Coordinator),
	    Parent ! {importance, Coordinator, Importance},
	    evaluation_coordinator(Parent, Coordinator, Processes);
	{exit, Parent} ->
	    Parent ! {done, self()}; %% TODO: lists:foreach(fun(Process) -> Process ! {exit, Coordinator} end, Processes)
	{Method, TreeFun, Parent}  ->
	    Result = submit_query(Method, TreeFun, Processes, Coordinator),
	    Parent ! {Method, Coordinator, Result},
	    evaluation_coordinator(Parent, Coordinator, Processes)		
    end.

%%
%% Transition every build process into an evaluator process
%%
transition_coordinator(Parent, Coordinator, 0, Acc) ->
    io:format(standard_error, "~n", []), % Note: separate progress (sorry)
    Parent ! {done, Coordinator},
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
build_coordinator(Parent, Sets, Cores) ->
    build_coordinator(Parent, self(), 1, Sets, Cores).

build_coordinator(Parent, Coordinator, Counter, Sets, Cores) when Sets < Counter->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid])
    end;
build_coordinator(Parent, Coordinator, Counter, Sets, Cores) ->
    receive 
	{build, Coordinator, Pid} ->
	    Pid ! {build, Counter},
	    build_coordinator(Parent, Coordinator, Counter + 1, Sets, Cores)
    end.

%%
%% Process for building bootap replicas and train "Base". Transitions
%% into 'base_evaluator_process', at {completed, Coordinator}
%%
base_build_process(Coordinator, Base, Features, Examples, Conf) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    base_build_process(Coordinator, Base, Features, Examples, Conf, dict:new(), []).

base_build_process(Coordinator, Base, Features, Examples, 
		   #rr_conf{base_learner={T,_}, progress=Progress, bagging=Bagger} = Conf, VariableImportance, Models) ->
    Coordinator ! {build, Coordinator, self()},
    receive
	{build, Id} ->
	    {Bag, _OutBag} = Bagger(Examples), %% NOTE: Use outbag for distributing missing values?
	    {Model, TreeVariableImportance, ImportanceSum} = Base:generate_model(Features, Bag, Conf),
	    
	    NewVariableImportance = update_variable_importance(TreeVariableImportance, VariableImportance, ImportanceSum),
	    ets:insert(models, {Id, Model}),
	    Rem = if T > 10 -> round(T/10); true -> 1 end,
	    case Id rem Rem of
		0 ->
		    Progress(Id, T);
		_ ->
		    ok
	    end,
	    base_build_process(Coordinator, Base, Features, Examples, Conf, NewVariableImportance, [{Id, Model}|Models]);
	{completed, Coordinator} ->
	    base_evaluator_process(Coordinator, self(), Base, Conf, VariableImportance, Models)
    end.

%%
%% Recives, {evaluate, Coordinator, ExId}, where "ExId" is an
%% example. The correct class for "ExId" is predicted using "Models"
%%
base_evaluator_process(Coordinator, Self, Base, Conf, VariableImportance, Models)->
    receive
	{evaluate, Coordinator, Self, ExId} ->
	    Coordinator ! {prediction, Coordinator, Self, make_prediction(Models, Base, ExId, Conf)},
	    base_evaluator_process(Coordinator, Self, Base, Conf, VariableImportance, Models);
	{importance, Coordinator, Self} ->
	    Coordinator ! {importance, Coordinator, Self, dict:to_list(VariableImportance)},
	    base_evaluator_process(Coordinator, Self, Base, Conf, VariableImportance, Models);
	{exit, Coordinator, Self} ->
	    done;  
	{Method, TreeFun, Coordinator, Self} ->
	    Coordinator ! {Method, Coordinator, Self, TreeFun(Models, Base, Conf)},
	    base_evaluator_process(Coordinator, Self, Base, Conf, VariableImportance, Models)
    end.

%%
%% Use models built using "Base" to predict the class of "ExId"
%%
make_prediction(Models, Base, ExId, Conf) ->
    make_prediction(Models, Base, ExId, Conf, []).

make_prediction([], _Base, _ExId, _Conf, Acc) ->
    Acc;
make_prediction([{ModelNr, Model}|Models], Base, ExId, Conf, Acc) ->
    {Prediction, NodeNr} = Base:predict(ExId, Model, Conf, []),
    make_prediction(Models, Base, ExId, Conf, [{Prediction, [ModelNr|NodeNr]}|Acc]).

update_variable_importance([], Acc, _) ->
    Acc;
update_variable_importance([{Feature, Importance}|Rest], Acc, Total) ->
    update_variable_importance(Rest, dict:update_counter(Feature, Importance/Total, Acc), Total);
update_variable_importance(TreeVariables, VariableImportance, Total) ->
    dict:fold(fun (Feature, Importance, Acc) ->
		      dict:update_counter(Feature, Importance/Total, Acc)
	      end, VariableImportance, TreeVariables).
    
