%%
%% Record for describing rules IF cond AND cond THEN class
%%   score       the score obtained by from evaluation of #rr_conf.rule,
%%               default is {} which is smaller than any number
%%   length      the length of the rule (i.e. the number of antecedents)
%%   covered     the coverage of the rule {Pos, Neg}
%%   antecedent  the list of conditions that must hold (IF-part) for the 
%%               consequent to be true
%%   consequent  the result when all conditions in antecedent holds
%% 
%%
-record(rr_rule, {score=-1000000, length = 0, covered, antecedent=[], consequent}).


%%
%% Record for describing candidate rules
%%   score    the score calculdated for a given candidate rule  
%%   value    the value (IF feature = value)
%%   covered  the examples covered by the candidate rule
%%
-record(rr_candidate, {score, value, covered}).
