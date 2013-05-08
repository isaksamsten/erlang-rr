%%
%% 
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
%% 
-record(rr_conf, {
	  prune  :: prune_fun(), 
	  depth = 0 :: integer(),
	  branch :: branch_fun(),
	  bagging,
	  score :: score_fun(),
	  split,
	  distribute :: distribute_fun(),
	  missing_values :: missing_fun(),
	  progress,
	  base_learner,
	  output,
	  cores = 1 :: integer(),
	  no_features :: integer(),
	  log,
	  payload :: any()
	 }).

%%
%% * score = score of current node
%% * feature = feature at split
%% * distribution = {......}
%% * left = node leading left
%% * right = node leadning right
%%
-record(rr_node, {id, 
		  score, 
		  feature, 
		  distribution :: {LeftCount::number(), RightCount::number(), Class::atom()},
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
		  class
		 }).

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


-type exid() :: Id::number() | {Id::number(), Id::number()}.
-type feature() :: {Type::atom(), Id::number()} | 
		   {rule, rule(), Lengt::number()} |
		   tuple().
-type features() :: [feature()].

-type example() :: {Class::atom(), Count::number(), Examples::[exid(),...]}.
-type examples() :: [example()].

-type prediction() :: {{Class::atom(), Score::number()}, NodeId::[number(),...]}.

-type split() ::  {left | right, examples()} | {both, examples(), examples()}.
-type missing_example() :: {left, exid()} | {right, exid()} | {both, exid(), exid()}.
-type distribute_example() :: {'?', Count::number()} | {left, Count::number()} | {right, Count::number()} |
			      {left, exid(), exid()} | {right, exid(), exid()} |
			      {both, exid(), exid()} | {all, exid(), exid(), exid()}.
			   
-type score() :: {Total::number(), Left::number(), Right::number()}.
-type classifier() :: {No::number(), Base::atom()}.
-type tree() :: #rr_node{} | #rr_leaf{}.
-type rule() :: {[{Feature::feature(), Value::atom()}, ...], Class::atom()}.

-type prune_fun() :: fun((examples(), Depth::number()) -> boolean()).
-type branch_fun() :: fun((features(), examples(), number(), #rr_conf{}) -> #rr_candidate{}). 
-type score_fun() :: fun((split(), Total::number()) -> score()).
-type distribute_fun() :: fun((feature(), exid()) -> distribute_example()).
-type missing_fun() :: fun((predict | build, feature(), exid(), Left::number(), Right::number()) -> missing_example()).
