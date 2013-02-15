-record(rr_conf, {prune, selection, depth}).

-record(rr_node, {score, feature, popularity, nodes}).
-record(rr_leaf, {purity, correct, incorrect, class}).
-record(rr_candidate, {feature, score, split}).
