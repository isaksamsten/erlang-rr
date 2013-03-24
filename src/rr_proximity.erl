%%% @author Isak Karlsson <isak@dhcp-159-52.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% Module for generating a proximity matrix from an induced model
%%% @end
%%% Created : 21 Mar 2013 by Isak Karlsson <isak@dhcp-159-52.dsv.su.se>
-module(rr_proximity).
-export([generate_proximity/3,
	 examples/1,
	 init/0]).
-include("rr_tree.hrl").

init() ->
    catch ets:delete(proximity),
    ets:new(proximity, [named_table, public, {read_concurrency, true}]).


examples(ExId0) ->
    ExId = rr_example:exid(ExId0),
    ets:lookup_element(proximity, ExId, 2).
    

generate_proximity(Model, Examples, #rr_conf{base_learner={Trees, _}} = Conf) ->
    Dict = generate_proximity(Model, Examples, Conf, dict:new()),
    Prox = generate_promixity(Dict, Trees),
    dict:fold(fun (I, V, _) ->
		      List0 = dict:fold(fun (J, Count, Acc) ->
						[{J, Count}|Acc]
					end, [], V),
		      List = lists:reverse(lists:keysort(2, List0)),
		      ets:insert(proximity, {I, List}) %% NOTE: only store a subset?
	      end, [], Prox),
    Model ! {exit, self()}.

generate_promixity(Dict, Trees) ->
    dict:fold(fun (_, Value, Acc) ->
		      lists:foldl(fun (I, Dict0) ->
					  lists:foldl(fun (J, Dict1) ->
							      if I =/= J ->
								      dict:update(I, fun (Dict2) ->
											  dict:update_counter(J, 1*(1/Trees), Dict2)
										  end, dict:store(J, 1, dict:new()), Dict1);
								 true ->
								      Dict1
							      end
						      end, Dict0, Value)
				  end, Acc, Value)
	      end, dict:new(), Dict).
					  
		  

generate_proximity(_, [], _, Acc) ->
    Acc;
generate_proximity(Model, [{_, _, ExIds}|Rest], Conf, Dict) ->
    NewDict = generate_proximity_for_class(Model, ExIds, Conf, Dict),
    generate_proximity(Model, Rest, Conf, NewDict).

generate_proximity_for_class(_, [], _Conf, Dict) ->
    Dict;
generate_proximity_for_class(Model, [ExId|Rest], Conf, Dict) ->
    Model ! {evaluate, self(), ExId},
    receive
	{prediction, Model, Predictions} ->
	    NewDict = lists:foldl(fun ({_P, NodeNr}, Acc) ->
					  dict:update(NodeNr,
						      fun (ExIds) ->
							      [ExId|ExIds]
						      end, [ExId], Acc)
				  end, Dict, Predictions),
	    generate_proximity_for_class(Model, Rest, Conf, NewDict)
    end.
