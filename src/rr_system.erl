%%% @author Isak Karlsson <isak@dhcp-159-51.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2013 by Isak Karlsson <isak@dhcp-159-51.dsv.su.se>

-module(rr_system).

-export([
	 save_model/3,
	 save_model/2,
	 load_model/1
	]).

-define(VERSION, '1.0').

save_model(Model, File, Opts) ->
    Compress = proplists:get_value(compress, Opts, true),
    ModelDump = [{file_version, ?VERSION},
		 {model, Model}],
    Dump = if Compress == true ->
		   term_to_binary(ModelDump, [compressed]);
	      true ->
		   term_to_binary(ModelDump)
	   end,
    file:write_file(File, Dump),
    ok.

save_model(Model, File) ->
    save_model(Model, File, []).

load_model(File) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    load_file(Binary);
	{error, Reason} ->
	    Reason
    end.

load_file(Binary) ->
    Model = binary_to_term(Binary),
    case proplists:get_value(file_version, Model) of
	?VERSION ->
	     proplists:get_value(model, Model);
	_ ->
	    {error, invalid_version}
    end.
