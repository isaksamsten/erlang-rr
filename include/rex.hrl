-include("rf.hrl").

-record(rex, {
	  classifier :: any(),
	  confidence :: float(),
	  coverage :: float()
	 }).

-record(rex_rule, {
	  a
	 }).
