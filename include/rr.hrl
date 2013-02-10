%%% @author Isak Karlsson <isak-kar@dsv.su.se>
%%% @copyright (C) 2013, Isak Karlsson
%%% @doc
%%% 
%%% @end
%%% Created : 10 Feb 2013 by Isak Karlsson <isak-kar@dsv.su.se>

%%
%% Describes the heuristics for evaluating rules:
%%  classes    number of classes in a multiclass problem
%%  evaluator  evaluation the goodness of a rule (default: rr_purity)
%%  search     returns the features for searching (default: rr_best)  
%%
-record(rr_heuristic, {original,
		       covered,
		       classes, 
		       eval,
		       search}).

%%
%% Record for describing candidate rules
%%
-record(rr_candidate, {score, value, covered}).

-record(rr_rule, {score=missing, coverage, antecedent, consequent}).

