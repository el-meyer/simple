#' @export

lPltfDsgn <- list()

# Initialize lRecrPars and get Accrual Rate Function
lPltfDsgn$lRecrPars <- lRecrPars(lambda = 4)
fnRecrFun  <- match.fun(lPltfDsgn$lRecrPars$fnRecrProc)

# while (trial_is_running) {
  
  dCurrTime <- NA
  dActvIntr <- NA
  
  # Call Accrual Function with default arguments + current Time and active Cohorts
  nNewPats <- 
    do.call(
      fnRecrFun, 
      args = list(
        lArgs     = lPltfDsgn$lRecrPars$lArgs, 
        dCurrTime = dCurrTime, 
        dActvIntr = dActvIntr
        )
      )
  
  # Don't add new ISAs if maximum number is already reached
  # nInclIntr <- NUMBER OF INCLUDED ISAs
  if (lIntrStartTime$nMaxIntr < nInclIntr) {
    
    # Initilize number of new ISAs to be added in this time step
    nNewIntr_temp <- 0
    
    # Check whether new ISA will be included randomly
    if (!is.null(lIntrStrtTime$dRandIntrIncl)) {
      if (
        base::sample(
          x = 0:1, 
          size = 1,
          prob = c(1 - lIntrStrtTime$dRandIntrIncl, lIntrStrtTime$dRandIntrIncl)
        ) == 1 ) {
        nNewIntr_temp <- nNewIntr_temp + 1
      }
    }
    
    # Make sure number of ISAs to be included is not going to overshoot maximum number of ISAs
    nNewIntr <- min(nNewIntr, lIntrStartTime$nMaxIntr - nInclIntr)
    
  }
  
# }
