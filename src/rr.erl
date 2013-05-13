%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%%
%%% @end
%%% Created :  4 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>
-module(rr).
-compile(export_all).
-author('isak-kar@dsv.su.se').


main([Hd|Args]) ->
    Cmd = list_to_atom(Hd),
    case Cmd of
	rf ->
	    rf:main(Args)		
    end;
main([]) ->
    ok.
