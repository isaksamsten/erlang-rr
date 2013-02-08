
-record(rr_heuristic, {pos_c, neg_c, pos, neg, classes, evaluator, apriori}).

-record(rr_candidate, {score, value, coverage}).

-record(rr_rule, {antecedent, consequent}).

