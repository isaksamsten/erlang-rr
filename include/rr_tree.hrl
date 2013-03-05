%%% @author  Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>


%%
%% Configuration and options for building the trees 
%%
%% * prune = function for prepruning (2 args)
%% * depth = current depth
%% * evaluate = function for evaluating potential split points (4 args)
%% * score = score function for scoring split points (2 args)
%% * split = function for splitting the data set (2 args)
%% * progress = function called for each 10 precent of trees built
%% * base_learner = tuple() -> {NumClassifiers, base_learner_module}
%% * cores = number of execution slots
%% * no_features = total number of features (currently)
%% * debug = function taking two arguments: (debug, error, or info) a string, 
%%           and optional arguments i.e. fun(.., Str, [])
%%
-record(rr_conf, {
	  prune, 
	  depth=0,
	  evaluate,
	  score,
	  split,
	  progress,
	  base_learner,
	  cores = 1,
	  no_features,
	  log
	 }).

%%
%% * score = score of current node
%% * feature = feature at split
%% * distribution = {Left, Right} where Left and Right are [{Class, Count}, ..., {Class, Count}]
%% * left = node leading left
%% * right = node leadning right
%%
-record(rr_node, {score, feature, distribution, left, right}).

%%
%% * score = score of leaf (e.g. laplace-estimated purity)
%% * distribution = fraction of {Correct, Incorrect}
%% * class = the class predicted by leaf
%%
-record(rr_leaf, {score, distribution, class}).

%%
%% * feature = the feature involving the split
%% * score = the score of this split (less is better)
%% * split = [Left, Right]
%%
-record(rr_candidate, {feature, score, split}).
