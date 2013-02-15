%%% @author  Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>


%%
%%
%%
-record(rr_conf, {prune, 
		  depth=0,
		  evaluate,
		  score,
		  base_learner,
		  classifiers={100, rr_tree},
		  cores = 1,
		  max_id}).

%%
%%
%%
-record(rr_node, {score, feature, nodes}).

%%
%%
%%
-record(rr_leaf, {score, distribution, class}).

%%
%%
%%
-record(rr_candidate, {feature, score, split}).
