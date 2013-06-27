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

    ./rebar get-deps compile escriptize
    
Then run as:

    ./rr rf -i data/iris.txt -n 100 -x cv --folds 10

'erlang-rr' can also be used as a framework in another application. 
For example, a file can be read and loaded and a model be
built and evaluated:

    File = csv:binary_reader("iris.txt"),
    {Features, Examples, Dataset} = rr_example:load(File, 4) %% on four cores
    {Build, Evaluate, _} = rf:new([{no_features, math:log(length(Features))/math:log(2)}])
    {Result, Models} = rr_eval:cross_validation(Features, Examples, Dataset,
                                                [{build, Build},
                                                {evaluate, Evaluate},
                                                {folds, 10}]),
    csv:kill(File), % clean up memory
    rr_example:kill(Dataset),
    lists:foreach(fun (Model) -> rf:kill(Model) end, Models),
    io:format("~p~n", [Res]).
    
To not fill up memory while doing cross-validation, models can be discarded as
they are generated.  To do this, wrap the evaluate-fun (from rf:new/1), in
another fun. For example,

    {Result, Models} = rr_eval:cross_validation(Features, Examples, Dataset,
                                                [{build, Build},
                                                {evaluate, fun (Model, Examples, ExConf) ->
                                                                Tmp = Evaluate(Model, Examples, ExConf),
                                                                rf:kill(Model),
                                                                Tmp
                                                           end},
                                                {folds, 10}]),
    %.....
