%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').

-define(DATE, "2013-02-10").
-define(MAJOR_VERSION, 0).
-define(MINOR_VERSION, 1).
-define(REVISION, 'beta-3').

-define(AUTHOR, "Isak Karlsson <isak-kar@dsv.su.se>").


cmd_spec() ->
    [{input_file,     $i,          "input",   string, 
      "Input data set"},
     {rulesets,       $m,          undefined, {integer, 10},
      "Number of rulesets to generate"}].

main(Args) ->
    Options = case getopt:parse(cmd_spec(), Args) of
		  {ok, Parsed} -> 
		      Parsed;
		  {error, _} ->
		      illegal()		      
	      end,
    InputFile = get_opt(input_file, fun illegal/0, Options),
    Classifiers = get_opt(rulesets, fun illegal/0, Options),
    Then = now(),
    Rules = rr_ruleset:test(InputFile, Classifiers),
    Now = now(),
    io:format("~p ~n", [Rules]),
    io:format(standard_error, "*** Generated ruleset in ~p second(s)*** ~n", 
	      [timer:now_diff(Now, Then) / 1000000]).
    
    

%%
%% Halts the program if illegal arguments are supplied
%%
illegal() ->
    getopt:usage(cmd_spec(), "rr"),
    halt().

%%
%% Get command line option Arg, calling Fun1 if not found
%%	     
get_opt(Arg, Fun1, {Options, _}) ->	
    case lists:keyfind(Arg, 1, Options) of
	{Arg, Ws} ->
	    Ws;
	false -> 
	    Fun1()
    end.

show_information() -> 
    io_lib:format("Rule learner, Version (of ~s) ~p.~p.~s ~nAll rights reserved ~s", 
		  [?DATE, ?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION, ?AUTHOR]).
