%%% @author Isak Karlsson <isak@dhcp-159-51.dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2013 by Isak Karlsson <isak@dhcp-159-51.dsv.su.se>

-module(rr_system).

-export([
	 serialize_model/2,
	 unserialize_model/1
	]).

-define(VERSION, '1.1').

serialize_model(Module, Model) ->
    ModelDump = [{file_version, ?VERSION},
		 {module, Module},
		 {model, Model}],    
    term_to_binary(ModelDump, [compressed]).

unserialize_model(Binary) ->
    Model = binary_to_term(Binary),
    case proplists:get_value(file_version, Model) of
	?VERSION ->
	    {proplists:get_value(module, Model),
	     proplists:get_value(model, Model)};
	_ ->
	    throw({error, invalid_version})
    end.
