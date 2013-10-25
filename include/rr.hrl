-record(classifier, {
          module :: atom(),
          model :: undefined | any(),
          config :: any()
         }).

-record(rr_ensemble, {
          model :: pid(),
          progress :: any(),
          bagging :: any(),
          base_learner :: {Module::atom(), Conf::any()},
          no_classifiers = 100 :: integer(),
          cores = 1 :: integer()          
         }).

-record(rr_base, {
          id,
          model,
          accuracy,
          no_rules
         }).

%% @doc
%% Dataset database for storing the complete vectors and other important
%% values related to a dataset, which don't need to carried around.
%%
%% In the current implementation (rr_example) store this data in ets-tables
%%
%% @end
-record(database, {
          examples :: number(),
          features :: number(),
          predictions :: number(),
          values :: number()
         }).

%% @doc
%% 
%% @end
-record(dataset, {
          target :: atom(),
          module :: atom(),
          features :: any(),
          no_features :: integer(),
          no_examples :: number(),
          examples :: any(),
          database :: #database{}
         }).

%%
%% * feature = the feature involving the split
%% * score = the score of this split (less is better)
%% * split = [Left, Right]
%%
-record(candidate, {
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
          predictions :: number(),
          values :: number()
         }).

-record(rr_exset, {
          features :: features(),
          examples :: examples(),
          exconf :: #rr_example{}
         }).

%% a particular example (with id)
-record(exid, {
          id :: integer(),
          count = 1 :: number() %% note: add weight etc.
         }).

-type exid() :: Id::number() | {Id::number(), Id::number()} | #exid{}.
-type feature() :: {Type::atom(), Id::number()} | 
                   {rule, rule(), Lengt::number()} |
                   tuple().
-type features() :: [feature()].

-type example_set() :: #rr_exset{}.
-type example() :: {Class::atom(), Count::number(), Examples::[exid(),...]}.
-type examples() :: [example()].

-type prediction() :: {{Class::atom(), Score::number()}, NodeId::[number(),...]}.

-type split() ::  {left | right, examples()} | {both, examples(), examples()}.
-type missing_example() :: {left, exid()} | {right, exid()} | {both, exid(), exid()}.
-type distribute_example() :: {'?', Count::number()} | 
                              {left, Count::number()} | 
                              {right, Count::number()} |
                              {left, Left::exid(), Missing::exid()} | 
                              {right, Rigth::exid(), Missing::exid()} |
                              {both, Left::exid(), Right::exid()} | 
                              {all, Left::exid(), Right::exid(), Missing::exid()}.
                           
-type score() :: {Total::number(), Left::number(), Right::number()}.
-type classifier() :: {No::number(), Base::atom()}.
-type rule() :: {[{Feature::feature(), Value::atom()}, ...], Class::atom()}.

-type distribute_fun() :: fun((feature(), exid()) -> distribute_example()).
-type missing_fun() :: fun((predict | build, feature(), exid(), Left::number(), Right::number()) -> missing_example()).

-type result_list() :: {atom(), any()} | {atom(), any(), any()}.
-type result() :: {{atom(), atom(), Model::any()}, [result_list(),...]}.
-type result_set() :: {cv, Folds::integer(), [result(),...]} | {split, result()} | {atom(), tuple()}.
