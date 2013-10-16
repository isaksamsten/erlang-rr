%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2013 by Isak Karlsson <isak-kar@dsv.su.se>

-module(rr_test).
-behaviour(rr_command).
-behaviour(rr_module).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
	 parse_args/1,
	 args/1
	]).

-export([
	 main/1,
	 help/0
	]).

-define(CMD_SPEC, 
	[{<<"evaluator">>, $e, "evaluation", {string, "cv --folds 10"},
	 "Choose the evalaution method"},
	 {<<"classifier">>, $c, "classifier", {string, "rf -n 10"},
	  "Choose the classification method"},
	 {<<"dataset">>, $i, "input", string,
	  "Specifies the input dataset in csv-format with rows of equal length. The first row must describe the type of attributes as 'numeric' or 'categoric' and exactly one 'class'. The second row name each attribute including the class. Finally, every row below the first two describe exactly one example."},
	 {<<"output">>,         $o,           "output",      {atom, default},
	  "Output format. Available options include: 'default' and 'csv'. If 'csv' is selected output is formated as a csv-file (see Example 5)"}
	]).
-define(NAME, "test").

parse_args(Args) ->
    rr:parse(?NAME, Args, ?CMD_SPEC).

help() ->
    rr:show_help(options, ?CMD_SPEC, ?NAME).

main(Args) ->
    Opts = args(Args, fun rr:illegal_option/2),
    Evaluator = proplists:get_value(evaluator, Opts),
    Classifier = proplists:get_value(classifier, Opts),
    Dataset = proplists:get_value(dataset, Opts),
    Output = proplists:get_value(output, Opts),
    Cores = erlang:system_info(schedulers),
    rr_log:info("loading '~s' on ~p core(s)", [Dataset, Cores]),
    LoadingTime = now(),
    Csv = csv:binary_reader(Dataset),
    Exset = rr_example:load(Csv, Cores),

    rr_log:debug("loading took ~p second(s)", [rr:seconds(LoadingTime)]),
    ExperimentTime = now(),
    {Res, _Model} = Evaluator(Exset, Classifier),
    Output(Res),
    rr_log:info("experiment took ~p second(s)", [rr:seconds(ExperimentTime)]),
    csv:kill(Csv),
    rr_example:kill(Exset),
    ok.

args(Args) ->
    args(Args, fun (Value, Reason) -> throw({bad_arg, ?NAME, Value, Reason}) end).

args(Args, Error) ->
    Evaluator = args(<<"evaluator">>, Args, Error),
    Classifier = args(<<"classifier">>, Args, Error),
    Dataset = args(<<"dataset">>, Args, Error),
    Output = args(<<"output">>, Args, Error),
    [{evaluator, Evaluator}, 
     {dataset, Dataset},
     {output, Output},
     {classifier, Classifier}].

args(Key, Opts, Error) ->
    Value = proplists:get_value(Key, Opts),
    case Key of
	<<"classifier">> ->
	    rr_classifier:find(Value);
	<<"evaluator">> ->
	    rr_evaluator:find(Value);
	<<"output">> ->
	    output(Value, Error);
	<<"dataset">> ->
	    Value
    end.

output(Value, Error) ->
    case Value of
	default -> rr_result:default();
	csv -> rr_result:csv();
	Other -> Error("output", Other)
    end.

-ifdef(TEST).

setup() ->
    rr_config:init([
		    {'rr.classifiers', [{"rf", rf, "he"}]},
		    {'rr.evaluators', [{"cv", cross_validation, "he"}]}
		   ]).

tear(_) ->
    rr_config:stop().

test_test_() ->
    {setup,
     fun setup/0,
     fun tear/1,
     [{"commands", ?_test(test_commands())}]}.

test_commands() ->
    Opts = parse_args(["-c", "rf -c 10", "-e", "cv --folds 10", "-i", "iris.txt"]),
    Args = args(Opts, fun(_, _) -> ok end),
    ?assertEqual(true, is_function(proplists:get_value(evaluator, Args))),
    ?assertEqual(true, is_function(proplists:get_value(output, Args))),
    ?assertEqual("iris.txt", proplists:get_value(dataset, Args)),
    ?assertEqual(true, is_function(proplists:get_value(build, proplists:get_value(classifier, Args)))),
    ?assertEqual(true, is_function(proplists:get_value(evaluate, proplists:get_value(classifier, Args)))).
-endif.





