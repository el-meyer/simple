
iterations <- 10
sim_res <- list()

for (i in 1:iterations) {
  
  sim_res[[i]] <- 
    fnRunSingleTrialSim(fnSimpleDesign())$lSummary
  
}


lOCs <- 
  list(
    Avg_Time                   = mean(sapply(sim_res, function(x) x$Total_Time)),
    Avg_Cohorts                = mean(sapply(sim_res, function(x) x$N_Cohorts)),
    Avg_TP                     = mean(sapply(sim_res, function(x) x$TP)),
    Avg_FP                     = mean(sapply(sim_res, function(x) x$FP)),
    Avg_TN                     = mean(sapply(sim_res, function(x) x$TN)),
    Avg_FN                     = mean(sapply(sim_res, function(x) x$FN)),
    Avg_any_P                  = mean(sapply(sim_res, function(x) x$any_P)),
    FDR                        = sum(sapply(sim_res, function(x) x$FP)) /
      sum(sapply(sim_res, function(x) x$TP) + sapply(sim_res, function(x) x$FP)),
    PTP                        = sum(sapply(sim_res, function(x) x$TP)) /
      sum((sapply(sim_res, function(x) x$TP) + sapply(sim_res, function(x) x$FN))),
    PTT1ER                     = sum(sapply(sim_res, function(x) x$FP)) /
      sum((sapply(sim_res, function(x) x$FP) + sapply(sim_res, function(x) x$TN)))
  )
