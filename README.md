# Warehouse Optimization

#  topics to discuss

1) VNS?
2) more complex routing?
3) ALNS: What does greedy heuristc mean? (p.16) used in heuristic 4; currently I predict execution time for every batch for all pickers and assign the respective fastest picker (very slow); 
4) less batching (and routing), more modeling?
5) shrinkage effect for random effects

# tbd

1) ALNS: score for predicted total batch execution times per picker. right now: algorithm runs until batches are distributed among 2-4 best pickers but maximum execution time limit per picker is not upheld; what happens when a picker's time limit is exceeded? 
2) ALNS: heuristics 1 + 2; hopefully 1.) fixes the others
