%%% @authorf Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_ensemble).
-author('isak-kar@dsv.su.se').
-compile(export_all). %% TODO: export
-define(VERSION, '1.0').

%% @headerfile "rf_tree.hrl"
-include("rf_tree.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc exit an ensemble builder set in evaluator state
kill(Model) ->
    Model ! {exit, self()}.

get_model(Model, Conf) ->
    Models = get_base_classifiers(Model),
    [{version, ?VERSION},
     {base_models, Models},
     {config, Conf}].
get_base_classifiers(Model) ->
    Collect = fun (BaseModels, _) -> BaseModels end,
    perform(Model, {collect_models, Collect, fun lists:append/2}).

load_model(Model) ->
    case proplists:get_value(version, Model) of
	?VERSION ->
	    Models = proplists:get_value(base_models, Model),
	    Conf = proplists:get_value(config, Model),
	    {load_evaluation_coordinator(Models, Conf#rr_ensemble.cores, Conf), Conf};
	_ ->
	    throw({error, invalid_version})
    end.

%% @doc load a set of base learners and put an ensemble in evaluator state
load_evaluation_coordinator(Models, Cores, Conf) ->
    Self = self(),
    Coordinator = spawn_link(fun() -> evaluation_coordinator(Self, self(), Conf) end),
    Partitions = rr_util:partition(Models, lists:map(fun(_) -> [] end, lists:seq(1, Cores))),
    Processes = lists:map(
		  fun (Part) ->
			  spawn_link(
			    fun() ->
				    base_evaluator_process(Coordinator, self(), Conf, undefined, Part)
			    end)
		  end, Partitions),
    Coordinator ! {start, Processes},
    Coordinator.

%% @doc generate an ensamble of models from of #rr_conf.base_learners
generate_model(Features, Examples, ExConf, Conf) ->
    #rr_ensemble{cores = Cores, no_classifiers = Classifiers} = Conf,
    Model = spawn_base_classifiers(Classifiers, Cores, Features, Examples, ExConf, Conf),
    receive
	{done, Model} ->
	    Model
    end.

%% @doc evaluate the performance of Model on Examples
evaluate_model(Models, Examples, ExConf, Conf) ->
    lists:foldl(fun ({Class, _, ExampleIds}, Acc) ->
			predict_all(Class, ExampleIds, Models, ExConf, Conf, Acc)
		end, dict:new(), Examples).

%% @doc get the variable importance
variable_importance(Model, #rr_ensemble{no_classifiers=Classifiers}) ->
    Model ! {importance, self()},
    receive
	{importance, Model, Importance} ->
	    update_variable_importance(Importance, dict:new(), Classifiers)
    end.

%% @doc get the out-of-bag base accuracy
oob_accuracy(Model, Conf) ->
    TreeFun = fun(BaseModels, _) ->
		      lists:foldl(fun ({_, _, A}, Acc) -> [A|Acc] end, [], BaseModels)
	      end,
    A = perform(Model, {oob_accuracy, TreeFun, fun lists:append/2}),
    lists:sum(A)/Conf#rr_ensemble.no_classifiers.

%% @doc
%% Calculate the base Test accuracy for each model. This function relies
%% on the (public) ets-table (predictions) to be filled o/w Second will contain [0.0,...], which
%% inhibits correct calucultation 
%% @end
-spec base_accuracy(any(), examples(), #rr_example{}, #rr_ensemble{}) -> 
			   {Average::float(), [{BaseAccuracy::float(), Second::float()},...]}.
base_accuracy(Model, Test, ExConf, Conf) ->
    SecondBestTest = lists:map(
		       fun ({Class, ExIds}) ->
			       {Class, length(ExIds), ExIds}
		       end, dict:to_list(
			      lists:foldl(
				fun (Dict, Acc) ->
					dict:merge(
					  fun (_Class, ExIdA, ExIdB) ->
						  ExIdA ++ ExIdB
					  end, Dict, Acc)
				end, dict:new(), 
				lists:foldl(
				  fun ({_Class, _, ExIds}, Acc) ->
					  [lists:foldl(
					     fun (ExId, New) ->
						     {_, Preds} = rr_example:get_prediction(ExConf, ExId),
						     dict:update(
						       if length(Preds) > 1 ->
							       {R, _Prob, _Votes} = lists:nth(2, Preds),
							       R;
							  true ->
							       undefined
						       end,
						       fun (NewExIds) ->
							       [ExId|NewExIds]
						       end, [ExId], New)
					     end, dict:new(), ExIds)|Acc]
				  end, [], Test)))),
    TreeFun = fun(BaseModels, #rr_ensemble{base_learner={Base, BaseConf}}) ->
		      lists:foldl(fun({_, BaseModel, _}, Acc) ->
					  Pred = Base:evaluate_model(BaseModel, Test, ExConf, BaseConf),
					  PredSecond = Base:evaluate_model(BaseModel, SecondBestTest, ExConf, BaseConf),
					  [{rr_eval:accuracy(Pred), rr_eval:accuracy(PredSecond)}|Acc]
				  end, [], BaseModels)
	      end,
    A = perform(Model, {base_accuracy, TreeFun, fun lists:append/2}),
    {lists:sum([Best || {Best, _} <- A])/Conf#rr_ensemble.no_classifiers, A}.

%% @doc perform an action on each model in the ensemble
perform(Model, {Method, TreeFun, CollectFun}) ->
    Model ! {q, Method, TreeFun, CollectFun, self()},
    receive
	{q, Method, Model, Return} ->
	    Return
    end.

%% @doc predict the class all examples
predict_all(_, [], _, _ExConf, _Conf, Dict) ->
    Dict;
predict_all(Actual, [Example|Rest], Model, ExConf, Conf, Dict) ->
    {Prediction, Probs} = predict_majority(Model, Example, ExConf, Conf),
    rr_example:insert_prediction(ExConf, Example, Probs),
    predict_all(Actual, Rest, Model, ExConf, Conf, dict:update(Actual, fun(Predictions) ->
								 [{Prediction, Probs}|Predictions]
							 end, [{Prediction, Probs}], Dict)).
%% @doc predict the class for Example
-spec predict_majority(pid(), exid(), #rr_example{}, #rr_ensemble{}) -> 
			      {Predition::tuple(), Rest::[]}.
predict_majority(Model, Example, ExConf, #rr_ensemble{no_classifiers=N}) ->
    Model ! {evaluate, self(), Example, ExConf},
    receive
	{prediction, Model, Predictions} ->
	    Probs = get_prediction_probabilities(Predictions, N),
	    {hd(Probs), Probs}
    end.


%% @doc ge the prediction probabilites for an example
get_prediction_probabilities(Predictions, N) ->
    Dict = lists:foldl(fun ({{Item, _Laplace, _Votes}, _NodeNr}, Dict) ->
			       dict:update(Item, fun(Count) -> Count + 1  end, 1, Dict)
		       end, dict:new(), Predictions),
    lists:sort(fun({_, Ca, _}, {_, Cb, _}) -> Ca > Cb end, 
	       lists:foldl(fun ({Class, Count}, Probs) ->
				   Votes = vote_list(Predictions, Class),
				   [{Class, Count/N, Votes}|Probs]
			   end, [], dict:to_list(Dict))).

vote_list(Predictions, Class) ->
    lists:map(fun ({{Item, _, _}, _}) ->
		      if Item == Class ->
			      1;
			 true ->
			      0
		      end
	      end, Predictions).

%% @doc Spaws classification and evaluator process
spawn_base_classifiers(Sets, Cores, Features, Examples, ExConf, Conf) ->
    Self = self(),
    Coordinator = spawn_link(fun() -> build_coordinator(Self, Sets, Cores, Conf) end),
    lists:foreach(
      fun(_) ->
	      spawn_link(
		fun() ->
			base_build_process(Coordinator, Features, Examples, ExConf, Conf)
		end)
      end, lists:seq(1, Cores)),
    Coordinator.
    

%% @doc collects a task 
collect_task(_, _, [], _, Acc) ->
    Acc;    
collect_task(Task, Collect, Processes, Coordinator, Acc) ->
    receive
	{Task, Coordinator, Pid, Predictions} ->
	    collect_task(Task, Collect, Processes -- [Pid], Coordinator, Collect(Acc, Predictions))
    end.

%% @doc submit a prediction task all the model evaluation process
submit_prediction(Processes, Coordinator, ExId, ExConf) ->
    lists:foreach(fun(Process) -> Process ! {evaluate, Coordinator, Process, ExId, ExConf} end, Processes),
    collect_task(prediction, fun lists:append/2, Processes, Coordinator, []).

%% @doc submit a process for collecting the variable importance from each model
submit_importance(Processes, Coordinator) ->
    lists:foreach(fun(Process) -> Process ! {importance, Coordinator, Process} end, Processes),
    collect_task(importance, fun lists:append/2, Processes, Coordinator, []).

%% @doc submit a query to every built model
submit_query(Method, TreeFun, CollectFun, Processes, Coordinator) ->
    lists:foreach(fun(Process) -> Process ! {q, Method, TreeFun, Coordinator, Process} end, Processes),
    collect_task({q, Method}, CollectFun, Processes, Coordinator, []).
	

%% @doc hijack evaluators
evaluation_coordinator(Parent, Coordinator, Conf) ->
    receive
	{start, Processes} ->
	    evaluation_coordinator(Parent, Coordinator, Processes, Conf)
    end.
		   
%% @doc coordinating the model evaluation
evaluation_coordinator(Parent, Coordinator, Processes, Conf) ->
    receive 
	{evaluate, Parent, ExId, ExConf} ->
	    Prediction = submit_prediction(Processes, Coordinator, ExId, ExConf),
	    Parent ! {prediction, Coordinator, Prediction},
	    evaluation_coordinator(Parent, Coordinator, Processes, Conf);
	{importance, Parent} ->
	    Importance = submit_importance(Processes, Coordinator),
	    Parent ! {importance, Coordinator, Importance},
	    evaluation_coordinator(Parent, Coordinator, Processes, Conf);
	{exit, Parent} ->
	    lists:foreach(fun(Process) -> Process ! {exit, Coordinator, Process} end, Processes),
	    Parent ! {done, self()};
	{q, Method, TreeFun, CollectFun, Parent}  ->
	    Result = submit_query(Method, TreeFun, CollectFun, Processes, Coordinator),
	    Parent ! {q, Method, Coordinator, Result},
	    evaluation_coordinator(Parent, Coordinator, Processes, Conf)		
    end.

%% @doc Transition every build process into an evaluator process
transition_coordinator(Parent, Coordinator, 0, Acc, Conf) ->
    Progress = Conf#rr_ensemble.progress,
    Progress(done, done),
    Parent ! {done, Coordinator}, %% signal that generate_model/3 can return
    evaluation_coordinator(Parent, Coordinator, Acc, Conf);
transition_coordinator(Parent, Coordinator, Cores, Acc, Conf) ->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid|Acc], Conf)
    end.

%% @doc coordinating the build process
build_coordinator(Parent, Sets, Cores, Conf) ->
    build_coordinator(Parent, self(), 1, Sets, Cores, Conf).

build_coordinator(Parent, Coordinator, Counter, Sets, Cores, Conf) when Sets < Counter->
    receive
	{build, Coordinator, Pid} ->
	    Pid ! {completed, Coordinator},
	    transition_coordinator(Parent, Coordinator, Cores - 1, [Pid], Conf)
    end;
build_coordinator(Parent, Coordinator, Counter, Sets, Cores, Conf) ->
    receive 
	{build, Coordinator, Pid} ->
	    Pid ! {build, Counter},
	    build_coordinator(Parent, Coordinator, Counter + 1, Sets, Cores, Conf)
    end.

%% @doc transision into 'base_evaluator_process', at {completed, Coordinator}
base_build_process(Coordinator, Features, Examples, ExConf, Conf) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    base_build_process(Coordinator, Features, Examples, ExConf, Conf, dict:new(), []).

base_build_process(Coordinator, Features, Examples, ExConf, Conf, VariableImportance, Models) ->
    #rr_ensemble{no_classifiers = T, base_learner={Base, BaseConf}, progress=Progress, bagging=Bagger} = Conf,
    Coordinator ! {build, Coordinator, self()},
    receive
	{build, Id} ->
	    {Bag, OutBag} = Bagger(Examples), %% NOTE: Use outbag for distributing missing values?
	    {Model, TreeVariableImportance, ImportanceSum} = Base:generate_model(Features, Bag, ExConf, BaseConf),
	    
	    NewVariableImportance = update_variable_importance(TreeVariableImportance, VariableImportance, ImportanceSum),
	    OOBAccuracy = case OutBag of
			      [] -> 0.0;
			      _-> rr_eval:accuracy(Base:evaluate_model(Model, OutBag, ExConf, BaseConf))
			  end,
	    Rem = if T > 10 -> round(T/10); true -> 1 end, %% todo: refactor (let progress decide)
	    case Id rem Rem of
		0 ->
		    Progress(Id, T);
		_ ->
		    ok
	    end,
	    base_build_process(Coordinator, Features, Examples, ExConf, 
			       Conf, NewVariableImportance, [{Id, Model, OOBAccuracy}|Models]);
	{completed, Coordinator} ->
	    base_evaluator_process(Coordinator, self(), Conf, VariableImportance, Models)
    end.

%% @doc enable inspection of generated models
base_evaluator_process(Coordinator, Self, Conf, VariableImportance, Models)->
    #rr_ensemble{base_learner={Base, BaseConf}} = Conf,
    receive
	{evaluate, Coordinator, Self, ExId, ExConf} -> 
	    Coordinator ! {prediction, Coordinator, Self, make_prediction(Models, Base, ExId, ExConf, BaseConf)},
	    base_evaluator_process(Coordinator, Self, Conf, VariableImportance, Models);
	{importance, Coordinator, Self} ->
	    Coordinator ! {importance, Coordinator, Self, dict:to_list(VariableImportance)},
	base_evaluator_process(Coordinator, Self, Conf, VariableImportance, Models);
	{exit, Coordinator, Self} ->
	    done;  
	{q, Method, TreeFun, Coordinator, Self} ->
	    Coordinator ! {{q, Method}, Coordinator, Self, TreeFun(Models, Conf)},
	    base_evaluator_process(Coordinator, Self, Conf, VariableImportance, Models)
    end.

%% @private use models built using "Base" to predict the class of "ExId"
make_prediction(Models, Base, ExId, ExConf, Conf) ->
    make_prediction(Models, Base, ExId, ExConf, Conf, []).

make_prediction([], _Base, _ExId, _ExConf, _Conf, Acc) ->
    Acc;
make_prediction([{ModelNr, Model, _}|Models], Base, ExId, ExConf, Conf, Acc) ->
    {Prediction, NodeNr} = Base:predict(ExId, Model, ExConf, Conf, []),
    make_prediction(Models, Base, ExId, ExConf, Conf, [{Prediction, [ModelNr|NodeNr]}|Acc]).

% @private update the variable mportance
update_variable_importance([], Acc, _) ->
    Acc;
update_variable_importance([{Feature, Importance}|Rest], Acc, Total) ->
    update_variable_importance(Rest, dict:update_counter(Feature, Importance/Total, Acc), Total);
update_variable_importance(TreeVariables, VariableImportance, Total) ->
    dict:fold(fun (Feature, Importance, Acc) ->
		      dict:update_counter(Feature, Importance/Total, Acc)
	      end, VariableImportance, TreeVariables).


-ifdef(TEST).

%partition_test() ->
%    Partitions = partition([1,2,3,4], [[], []]),
 %   ?assertEqual([[3,1], [4,2]], Partitions).

-endif.
