#' Creates a simple platform trial design of class lPltfDsgn
#'
#' @param endpoint           Type of endpoint; either "binary" or "continuous"
#' 
#' @param control_effect     Effect of control treatment. Either response rate if endpoint is
#'                           binary or mean if endpoint is continuous.
#' 
#' @param effect_size        If endpoint is binary, defined as odds-ratio. 
#'                           If endpoint is continuous, defined as difference in means 
#'                           (standard deviation is assumed to be 1)
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
#'     control_effect = 0.1,
#'     effect_size    = 2,
#'     sample_size    = 100,
#'     intr_at_start  = 3,
#'     sig_level      = 0.05,
#'     data_sharing   = "all",
#'     max_intr       = 10,
#'     obs_lag        = 10,
#'     recr_speed     = 5
#' )
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn)
#'
#' @export
fnSimpleDesign <- function(
  endpoint       = "binary",
  control_effect = 0.1,
  effect_size    = 2,
  sample_size    = 100,
  intr_at_start  = 3,
  sig_level      = 0.05,
  data_sharing   = "all",
  max_intr       = 10,
  obs_lag        = 10,
  recr_speed     = 5
) {
  
  # Create all sort of warning messages and stop messages
  
  # Helper function
  risk_plus_or <- function(
    risk, 
    or
  ) {
    odds <- risk/(1-risk)
    new_odds <- odds * or
    new_risk <- new_odds / (1 + new_odds)
    return(new_risk)
  }
  
  if (endpoint == "binary") {
    dTheta <- c(control_effect, risk_plus_or(control_effect, effect_size))
    dSigma <- NULL
  } else {
    dTheta <- c(control_effect, control_effect + effect_size)
    dSigma <- c(1, 1)
  }
  
  lIntrDsgn <- 
    c(
      rep(
        list(
          list(
            lInitIntr       = lInitIntr(cIntrName = "", cArmNames = c("C", "T"), nMaxNIntr = sample_size),
            lAllocArm       = lAllocArm(),
            lPatOutcome     = lPatOutcome(cGroups = c("C", "T"), dTheta = dTheta, dSigma = dSigma, dTrend = 0, nLag = obs_lag),
            lCheckAnlsMstn  = lCheckAnlsMstn(),
            lAnls           = lAnls(group1 = c("C", data_sharing), group2 = c("T", "Intr")),
            lSynthRes       = lSynthRes(alpha = sig_level),
            lCheckEnrl      = lCheckEnrl()
          )
        ),
        max_intr
      )
    )
  
  lPltfDsgn <- 
    lPltfDsgn(
      lAddIntr      = lAddIntr(intr_at_start),
      lAddPats      = lAddPats(),
      lAllocIntr    = lAllocIntr(),
      lIntrDsgn     = lIntrDsgn,
      lNewIntr      = lNewIntr(max_intr),
      lPltfSummary  = lPltfSummary(),
      lRecrPars     = lRecrPars(recr_speed),
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc()
    )
  
  return(lPltfDsgn)
  
}
