%%% @author  Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>


%%
%% @doc
%%
%% Configuration and options for building the trees 
%%
%% * prune = function for prepruning (2 args)
%% * depth = current depth
%% * branch = function for evaluating potential split points (4 args)
%% * bagging = function for generating a random sample for each tree (1 arg) (default: bagging)
%% * score = score function for scoring split points (2 args)
%% * split = function for splitting the data set (3 args)
%% * distribute = function for distributing values (2 args)
%% * missing_values = function for distributing missing values (5 args)
%% * progress = function called for each 10 precent of base_classifers built
%% * base_learner = tuple() -> {NumClassifiers, base_learner_module}
%% * cores = number of execution slots
%% * no_features = total number of features (currently)
%% * log = function taking two arguments: (debug, error, or info) a string, 
%%         and optional arguments i.e. fun(.., Str, [])
%%
%% @end
-record(rr_conf, {
	  prune  :: prune_fun(), 
	  depth=0 :: integer(),
	  branch,
	  bagging,
	  score,
	  split,
	  distribute,
	  missing_values,
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
-record(rr_node, {id, 
		  score, 
		  feature, 
		  distribution, 
		  left, 
		  right}).

%%
%% * score = score of leaf (e.g. laplace-estimated purity)
%% * distribution = fraction of {Correct, Incorrect}
%% * class = the class predicted by leaf
%%
-record(rr_leaf, {id, 
		  score, 
		  distribution, 
		  class}).

%%
%% * feature = the feature involving the split
%% * score = the score of this split (less is better)
%% * split = [Left, Right]
%%
-record(rr_candidate, {
	  feature :: feature(), 
	  score :: score(), 
	  split :: split()
	 }).


-type exid() :: number() | {number(), number()}.
-type feature() :: {atom(), number()} | 
		   {rule, [{feature(), atom()}, ...], number()} |
		   tuple().
-type features() :: [feature()].

-type example() :: {atom(), number(), [exid(),...]}.
-type examples() :: [example()].

-type prediction() :: {{atom(), number()}, [number(),...]}.

-type split() ::  {left | right, examples()} | {both, examples(), examples()}.
-type missing_example() :: {left, exid()} | {right, exid()} | {both, exid(), exid()}.
-type distribute_example() :: {'?', number()} | {left, number()} | {right, number()} |
			      {left, exid(), exid()} | {right, exid(), exid()} |
			      {both, exid(), exid()} | {all, exid(), exid(), exid()}.
			   
-type score() :: {number(), number(), number()}.
-type classifier() :: {number(), atom()}.
-type tree() :: #rr_node{} | #rr_leaf{}.

-type prune_fun() :: fun((examples(), number()) -> boolean()).
-type branch_fun() :: fun((features(), examples(), number(), #rr_conf{}) -> #rr_candidate{}). 
-type score_fun() :: fun((split(), number()) -> score()).
-type distribute_fun() :: fun((feature(), exid()) -> distribute_example()).
-type missing_fun() :: fun((predict | build, feature(), exid(), number(), number()) -> missing_example()).

