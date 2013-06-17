%%% @author  <Isak@ISAK-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  4 Jun 2013 by  <Isak@ISAK-PC>

-module(km).
-compile(export_all).

-define(E, 2.718281828459045).
-include("km.hrl").

-define(CMD_SPEC,
	[{help,           $h,           "help",         undefined,
	  "Show this usage information."},
	 {version,        undefined,    "version",      undefined,
	  "Show the program version."},
	 {examples,       undefined,    "examples",     undefined,
	  "View example usages"},
	 {input,          $i,           "input",        string, 
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."}
	 ]).

new(Props) ->
    K = proplists:get_value(clusters, Props, 10),
    Iterations = proplists:get_value(iterations, Props, 100),
    Conf = #km{k = K,
	       iterations = Iterations},
    Build = fun (Features, Examples) ->
		    kmeans:kmean(Features, Examples, Conf)
	    end,
    Evaluate = fun (_, _) ->
		       ok
	       end,
    {Build, Evaluate, Conf}.    

kill(_) ->
    ok.

help() ->
    ok.

main(Args) ->
    rr_example:init(),
    kmeans:init(),

    Options = rr:parse(Args, ?CMD_SPEC),
    InputFile = case proplists:get_value(input, Options) of
		    undefined ->
			rr:illegal("no input file defined"),
			halt();
		    F -> F
		end,

    File = csv:binary_reader(InputFile),
    {Features, Examples} = rr_example:load(File, 4),
    Centroids = kmeans:kmean(Features, Examples, #km{k=40}),
    lists:foreach(fun ({centroid, X}) ->
			  io:format("~p ~n", [ets:lookup(centroids, X)])
		  end, Centroids),
    io:format("~p ~n", [Centroids]),
    
    {Build, Evaluate, _} = rf:new([{no_features, round(math:log(40)/math:log(2)) + 1},
				   {distribute, distribute(Features)},
				   {split, split(Features)},
				   {no_trees, 100}]),
    Res = rr_eval:split_validation(Centroids, Examples, [{build, Build}, 
							 {evaluate, Evaluate}, 
							 {ratio, 0.66}]),
    io:format("~p ~n", [Res]).

distribute(Features) ->
    fun({{centroid, Id}, Threshold}, ExId) ->
	    Distance = exp(-1.0 * kmeans:euclidian(Features, rr_example:exid(ExId), Id)),
	    if Distance >= Threshold ->
		    {left, rr_example:count(ExId)};
	       true ->
		    {right, rr_example:count(ExId)}
	    end
    end.

sample_split_value(Features) ->
    fun ({centroid, Id}, Examples) ->
	    {Ex1, Ex2} = rr_example:sample_example_pair(Examples),
	    D1 = exp(-1.0 * kmeans:euclidian(Features, rr_example:exid(Ex1), Id)),
	    D2 = exp(-1.0 * kmeans:euclidian(Features, rr_example:exid(Ex2), Id)),
	    (D1 + D2) / 2
    end.

split(Features) ->
    Sample = sample_split_value(Features),
    fun(Feature, Examples, Distribute, Missing) ->
	    rr_example:split(Feature, Examples, Distribute, Missing, Sample)
    end.

exp(X) ->
    math:pow(?E, X).
