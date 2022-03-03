#' Creates a simple platform trial design of class lPltfDsgn
#'
#' @param endpoint           Type of endpoint; either "binary" or "continuous"
#' 
#' @param effects            Vector of length two giving the effects of the treatments. 
#'                           Either response rates if endpoint is binary or 
#'                           means if endpoint is continuous.
#'                           
#' @param sd                 If endpoint is continuous, a vector of length two
#'                           giving the standard deviation in both groups.
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
#'     effects        = c(0.1, 0.2),
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
#' out <- fnRunSingleTrialSim(fnSimpleDesign())
#' 
#' lPltfDsgn2 <- fnSimpleDesign(
#'     endpoint       = "continuous",
#'     effects        = c(1, 1.5),
#'     sd             = c(2, 2.2),
#'     sample_size    = 100,
#'     intr_at_start  = 3,
#'     sig_level      = 0.05,
#'     data_sharing   = "all",
#'     max_intr       = 10,
#'     obs_lag        = 10,
#'     recr_speed     = 5
#' )
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn2)
#'
#' @export
fnSimpleDesign <- function(
  endpoint       = "binary",
  effects        = c(0.1, 0.2),
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
  
  lIntrDsgn <- 
    c(
      rep(
        list(
          list(
            lInitIntr       = lInitIntr(cIntrName = "", cArmNames = c("C", "T"), nMaxNIntr = sample_size),
            lAllocArm       = lAllocArm(),
            lPatOutcome     = lPatOutcome(cGroups = c("C", "T"), dTheta = effects, dSigma = sd, dTrend = 0, nLag = obs_lag),
            lCheckAnlsMstn  = lCheckAnlsMstn(),
            lAnls           = lAnls(endpoint = endpoint, group1 = c("C", data_sharing), group2 = c("T", "Intr")),
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
