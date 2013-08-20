-record(rr_ensemble, {
	  progress,
	  bagging :: any(),
	  base_learner :: {Module::atom(), Conf::any()},
	  no_classifiers = 100 :: integer(),
	  cores = 1 :: integer(),
	  seed = {0,0,0} :: {integer(), integer(), integer()}
	 }).

%%
%% * feature = the feature involving the split
%% * score = the score of this split (less is better)
%% * split = [Left, Right]
%%
-record(rr_candidate, {
	  feature :: {Feature::feature(), Value::atom()},
	  score :: score(), 
	  split :: split()
	 }).

%%
-record(rr_rule, {
	  branch,
	  score,
	  split,
	  distribute,
	  missing_values
	 }).

%% references to data storages
-record(rr_example, {
	  examples :: number(),
	  features :: number(),
	  predictions :: number()
	 }).

%% record of examples of class class
-record(examples, {
	  class :: atom(),
	  count :: number(),
	  exids :: [exid(),...]
	 }.

%% a particular example (with id)
-record(exid, {
	  id :: integer(),
	  count = 1 :: number() %% note: add 
	 }.

-type exid() :: Id::number() | {Id::number(), Id::number()} | #exid{}.
-type feature() :: {Type::atom(), Id::number()} | 
		   {rule, rule(), Lengt::number()} |
		   tuple().
-type features() :: [feature()].

-type example() :: {Class::atom(), Count::number(), Examples::[exid(),...]} | #examples{}.
-type examples() :: [example()].

-type prediction() :: {{Class::atom(), Score::number()}, NodeId::[number(),...]}.

-type split() ::  {left | right, examples()} | {both, examples(), examples()}.
-type missing_example() :: {left, exid()} | {right, exid()} | {both, exid(), exid()}.
-type distribute_example() :: {'?', Count::number()} | {left, Count::number()} | {right, Count::number()} |
			      {left, exid(), exid()} | {right, exid(), exid()} |
			      {both, exid(), exid()} | {all, exid(), exid(), exid()}.
			   
-type score() :: {Total::number(), Left::number(), Right::number()}.
-type classifier() :: {No::number(), Base::atom()}.
-type rule() :: {[{Feature::feature(), Value::atom()}, ...], Class::atom()}.

-type distribute_fun() :: fun((feature(), exid()) -> distribute_example()).
-type missing_fun() :: fun((predict | build, feature(), exid(), Left::number(), Right::number()) -> missing_example()).

-type result_list() :: {atom(), any()} | {atom(), any(), any()}.
-type result() :: {{atom(), atom(), Model::any()}, [result_list(),...]}.
-type result_set() :: {cv, Folds::integer(), [result(),...]} | {split, result()} | {atom(), tuple()}.
