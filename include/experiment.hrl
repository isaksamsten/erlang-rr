-include("rr.hrl").

-record(experiment, {
	  datasets :: [],
	  evaluate :: any(),
	  classifier :: any(),
	  output :: any(),
	  iterations :: integer(),
	  progress :: any(),
	  loader :: any()
	 }).
