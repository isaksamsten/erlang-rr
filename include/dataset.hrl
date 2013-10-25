%% @doc
%% Dataset database for storing the complete vectors and other important
%% values related to a dataset, which don't need to carried around.
%%
%% In the current implementation (rr_example) store this data in ets-tables
%%
%% @end
-include("rr.hrl").

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
	  features :: features(),
          no_features :: integer(),
          no_examples :: number(),
	  examples :: examples(),
	  database :: #database{}
	 }).
