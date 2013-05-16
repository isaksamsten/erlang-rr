erlang-rr
=========

rr (ensamble learner), is both a framework for implementing machine
learning techniques in erlang, especially targeted against ensemble
techniques and a system implementing the random forest algorithm. It
provides a unified framework for handling examples, features and
ensembles, both supporting concurrency (if supported by the
algorithm).

Generally, the application can be used in two ways, either as a
stand-alone application compiled as:

    ./rebar compile escriptize

or as a framework in another application:

    File = csv:binary_reader("data.txt"),
    {Features, Examples} = rr_example:load(File, 4) %% on four cores
    {Build, Evaluate, _} = rf:new([{no_features, math:log(length(Features))/math:log(2)}])
    Res = rr_eval:cross_validation(Features, Examples, [{build, Build},
                                                        {evaluate, Evaluate},
							{folds, 10}]),
    io:format("~p~n", [Res]).