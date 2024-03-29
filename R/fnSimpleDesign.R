#' Creates a simple platform trial design of class lPltfDsgn
#'
#' @param endpoint           Type of endpoint; either "binary" or "continuous"
#' 
#' @param contr_eff          Control treatment effect. Either response rate if endpoint is binary or 
#'                           mean if endpoint is continuous.
#' 
#' @param trt_effs           Vector of length max_intr giving the treatment effects. 
#'                           Either response rates if endpoint is binary or 
#'                           means if endpoint is continuous.
#'                           
#' @param sd                 If endpoint is continuous, a vector of length two
#'                           giving the standard deviation in the control groups (first entry)
#'                           and the experimental groups (second entry).
#'
#' @param sample_size        Sample size per arm in the trial
#' 
#' @param intr_at_start      Number of ISAs at platform initiation
#' 
#' @param sig_level          Significance level to be used in statistical tests
#' 
#' @param data_sharing       Type of control data sharing to be used - either "All" (pool all data) or 
#'                           "Intr" (use only within ISA data) or "Conc" (use all concurrently collected data)
#'                           
#' @param max_intr           Maximum number of ISAs to be included in platform trial
#' 
#' @param obs_lag            How long does it take for outcomes to be observed?
#' 
#' @param recr_speed         How many patients per time unit are enrolled?
#'
#'
#' @return Object of class lPltfDsgn to be supplied to fnRunSingleTrialSim or modified later
#'
#' @examples
#' 
#' lPltfDsgn <- fnSimpleDesign(
#'     endpoint       = "binary",
#'     contr_eff      = 0.1,
#'     trt_effs       = rep(0.2, 5),
#'     sample_size    = 100,
#'     intr_at_start  = 3,
#'     sig_level      = 0.05,
#'     data_sharing   = "all",
#'     max_intr       = 5,
#'     obs_lag        = 10,
#'     recr_speed     = 5
#' )
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn)
#' out <- fnRunSingleTrialSim(fnSimpleDesign())
#' 
#' lPltfDsgn2 <- fnSimpleDesign(
#'     endpoint       = "continuous",
#'     contr_eff      = 1,
#'     trt_effs       = c(1, 1.5, 2),
#'     sd             = c(2, 2.2),
#'     sample_size    = 100,
#'     intr_at_start  = 2,
#'     sig_level      = 0.05,
#'     data_sharing   = "all",
#'     max_intr       = 3,
#'     obs_lag        = 10,
#'     recr_speed     = 5
#' )
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn2)
#'
#' @export
fnSimpleDesign <- function(
  endpoint       = "binary",
  contr_eff      = 0.1,
  trt_effs       = rep(0.2, max_intr),
  sd             = NULL,
  sample_size    = 100,
  intr_at_start  = 3,
  sig_level      = 0.05,
  data_sharing   = "all",
  max_intr       = 10,
  obs_lag        = 10,
  recr_speed     = 5
) {
  
  # Create all sort of warning messages and stop messages
  
  
  # add all ISAs separately
  lIntrDsgn <- list()
  for (i in 1:max_intr) {
    lIntrDsgn[[i]] <- 
      list(
        lInitIntr       = lInitIntr(cIntrName = "", cArmNames = c("C", "T"), nMaxNIntr = sample_size),
        lAllocArm       = lAllocArm(),
        lPatOutcome     = lPatOutcome(cGroups = c("C", "T"), dTheta = c(contr_eff, trt_effs[i]), dSigma = sd, dTrend = 0, nLag = obs_lag),
        lCheckAnlsMstn  = lCheckAnlsMstn(),
        lAnls           = lAnls(endpoint = endpoint, group1 = c("C", data_sharing), group2 = c("T", "Intr")),
        lSynthRes       = lSynthRes(alpha = sig_level),
        lCheckEnrl      = lCheckEnrl()
      )
  }
  
  
  
  lPltfSummary_simpleDesign <- 
    new_lPltfSummary(
      
      fnPltfSummary = function(lPltfTrial, lAddArgs) {
        
        # Define truth via:
        # Is RR1 > RR2
        
        truth <- rep(NA, length(lPltfTrial$isa))
        
        for (i in 1:length(truth)) {
            truth[i] <-
              trt_effs[i] > contr_eff
        }
        
        # Check which decisions were correct positives, false positives, etc.
        cp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" &  truth)
        fp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" & !truth)
        cn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" & !truth)
        fn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" &  truth)
        
        # Prepare return list
        ret <- list(
          Decision               = sapply(lPltfTrial$isa, function(x) x$cEndReason),
          Start_Time             = sapply(lPltfTrial$isa, function(x) x$nStartTime),
          Conr_Eff               = lAddArgs$contr_eff,
          Trr_Effs               = lAddArgs$trt_effs,
          N_Cohorts              = length(lPltfTrial$isa),
          Total_Time             = lPltfTrial$lSnap$dCurrTime,
          TP                     = cp,
          FP                     = fp,
          TN                     = cn,
          FN                     = fn,
          FDR_Trial              = ifelse(!is.na(fp/(cp + fp)), fp/(cp + fp), NA),
          PTP_Trial              = ifelse(!is.na(cp/(cp + fn)), cp/(cp + fn), NA),
          PTT1ER_Trial           = ifelse(!is.na(fp/(fp + cn)), fp/(fp + cn), NA),
          any_P                  = as.numeric((cp + fp) > 0)
        )
        
        return(ret)
        
      },
      # Pass true response rates to summary function
      lAddArgs   = list(
        contr_eff = contr_eff,
        trt_effs  = trt_effs
      )
    )
  
  lOCSynth_simpleDesign <- 
    new_lOCSynth(
      
      fnOCSynth = function(lIndTrials, lAddArgs) {
        
        lOCs <- 
          list(
            Avg_Time                   = mean(sapply(lIndTrials, function(x) x$Total_Time)),
            Avg_Cohorts                = mean(sapply(lIndTrials, function(x) x$N_Cohorts)),
            Avg_TP                     = mean(sapply(lIndTrials, function(x) x$TP)),
            Avg_FP                     = mean(sapply(lIndTrials, function(x) x$FP)),
            Avg_TN                     = mean(sapply(lIndTrials, function(x) x$TN)),
            Avg_FN                     = mean(sapply(lIndTrials, function(x) x$FN)),
            Avg_any_P                  = mean(sapply(lIndTrials, function(x) x$any_P)),
            FDR                        = sum(sapply(lIndTrials, function(x) x$FP)) /
              sum(sapply(lIndTrials, function(x) x$TP) + sapply(lIndTrials, function(x) x$FP)),
            PTP                        = sum(sapply(lIndTrials, function(x) x$TP)) /
              sum((sapply(lIndTrials, function(x) x$TP) + sapply(lIndTrials, function(x) x$FN))),
            PTT1ER                     = sum(sapply(lIndTrials, function(x) x$FP)) /
              sum((sapply(lIndTrials, function(x) x$FP) + sapply(lIndTrials, function(x) x$TN)))
          )
        
        return(lOCs)
        
      },
      
      lAddArgs   = list()
      
    )
  
  
  lPltfDsgn <- 
    lPltfDsgn(
      lAddIntr      = lAddIntr(),
      lAddPats      = lAddPats(),
      lAllocIntr    = lAllocIntr(),
      lIntrDsgn     = lIntrDsgn,
      lNewIntr      = lNewIntr(nMaxIntr = max_intr, nStartIntr = intr_at_start),
      lOCSynth      = lOCSynth_simpleDesign,
      lPltfSummary  = lPltfSummary_simpleDesign,
      lRecrPars     = lRecrPars(recr_speed),
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc()
    )
  
  return(lPltfDsgn)
  
}
