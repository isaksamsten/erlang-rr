-include("rf_tree.hrl").

-record(rex, {
	  classifier :: any(),
	  confidence :: float(),
	  coverage :: float()
	 }).

-record(rex_rule, {
	  a
	 }).
