-include("rr.hrl").

-record(rf_tree, {
          prune  :: prune_fun(),
          pre_prune :: pre_prune_fun(),
          depth = 0 :: integer(),
          branch :: branch_fun(),
          score :: score_fun(),
          split :: any(),
          distribute :: distribute_fun(),
          missing_values :: missing_fun(),
          no_features :: no_features_fun(),
          vi :: list()
         }).

%%
%% * score = score of current node
%% * feature = feature at split
%% * distribution = {......}
%% * left = node leading left
%% * right = node leadning right
%%
-record(rf_node, {id, 
                  score, 
                  feature, 
                  distribution :: {LeftCount::number(), 
                                   RightCount::number(), 
                                   Class::atom()},
                  left, 
                  right}).

%%
%% * score = score of leaf (e.g. laplace-estimated purity)
%% * distribution = fraction of {Correct, Incorrect}
%% * class = the class predicted by leaf
%%
-record(rf_leaf, {id, 
                  score, 
                  distribution, 
                  class
                 }).

-type tree() :: #rf_node{} | #rf_leaf{}.
-type prune_fun() :: fun((examples(), Depth::number()) -> boolean()).
-type pre_prune_fun() :: fun((split(), examples(), number()) -> boolean()).
-type branch_fun() :: fun((features(), examples(), number(), #rf_tree{}) -> 
                                 #rr_candidate{}). 
-type score_fun() :: fun((split(), Total::number()) -> score()).
-type no_features_fun() :: fun((TotalNoFeatures::number()) -> 
                                NewNoFeatures::number()).
-type branch() :: {BestSplit::#rr_candidate{}, NewFeatures::features()}.
-type unpacked_branch() :: BestSplit::#rr_candidate{}.
