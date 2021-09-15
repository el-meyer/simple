#' List of Function Definitions
#' 
#' Different ISA Designs to draw from of class lIntrDsgnList
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lFnDef
#' 
#' @export
#' @rdname lFnDef
# Constructor Function
new_lIntrDsgn <- function(
  
) {
  structure(
    list(
      fnGenNewPatData = function(
        lPltfDsgn,
        lPltfTrial, 
        dCurrTime
      ) {},
      fnAddNewIntr    = function(
        lPltfDsgn,
        lPltfTrial, 
        dCurrTime
      ) {}
    ),
    class       = "lFnDef"
  )
}
#' @export
#' @rdname lFnDef
# Validator Function
validate_lFnDef <- function(x) {
  
}
#' @export
#' @rdname lFnDef
# Helper Function
lFnDef <- function() {
  
  new_lFnDef(
    
    # fnGenNewPatData -------------
    fnGenNewPatData = function(lPltfDsgn, lPltfTrial, dCurrTime) {
      
      # Get Accrual Rate Function
      fnRecrProc  <- match.fun(lPltfDsgn$lRecrPars$fnRecrProc)
      
      # Call Accrual Function with default arguments + current Time and number of active interventions
      # no negative patients can be included
      nNewPats <- 
        max(
          0,
          round(
            do.call(
              fnRecrProc, 
              args = list(
                lArgs     = lPltfDsgn$lRecrPars$lArgs, 
                dCurrTime = dCurrTime, 
                dActvIntr = dActvIntr
              )
            )
          )
        )
      
      # WHAT TO DO with nNewOPats?? -------------
      },
    
    # fnAddNewIntr ---------------
    fnAddNewIntr = function(lPltfDsgn, lPltfTrial, dCurrTime) {
      
      #  STILL TO CALCULATE from lPltfTrial -----------
      nInclIntr <- 0 # Number of ISAs included so far
      nExitIntr <- 0  # Number if ISAs that exited platform this time step
      
      # Don't add new ISAs if maximum number is already reached
      # nInclIntr <- NUMBER OF INCLUDED ISAs
      if (lPltfDsgn$lIntrIncl$nMaxIntr > nInclIntr) {
        
        # Initilize number of new ISAs to be added in this time step
        # add as any as left the platform trial or 0
        nNewIntr_replace <- ifelse(lPltfDsgn$lIntrIncl$bIntrRepl, nExitIntr, 0) 
        
        # Get Accrual Rate Function
        fnAddNewIntr  <- match.fun(lPltfDsgn$lIntrIncl$fnAddNewIntr)
        
        # STILL TO CALCULATE FROM lPltfTrial -------------
        dLastAddTime <- 0 # Last Time an ISA was added
        nPatsPostAdd <- 0 # Number of Patients since last ISA was added
        
        nNewIntr_extra <- 
          round(
            do.call(
              fnAddNewIntr, 
              args = list(
                lArgs        = lPltfDsgn$lIntrIncl$lArgs, 
                dCurrTime    = dCurrTime, 
                dActvIntr    = dActvIntr,
                dLastAddTime = dLastAddTime,
                nPatsPostAdd = nPatsPostAdd
              )
            )
          )
        
        nNewIntr <- nNewIntr_replace + nNewIntr_extra
        
        # Make sure the number of ISAs included is not going to overshoot the maximum number of ISAs that can be added per timestep
        nNewIntr <- min(nNewIntr, lPltfDsgn$lIntrIncl$nMaxAdd)
        
        # ADD DIFFERENCE TO A "QUEUE" ------------
        # NEED TO ADD CONCEPT OF MAXIMUM ARMS IN PARALLEL
        
        # Make sure number of ISAs to be included is not going to overshoot maximum number of ISAs that can be added during the platform trial
        nNewIntr <- min(nNewIntr, lPltfDsgn$lIntrIncl$nMaxIntr - nInclIntr)
        
      } else {
        nNewIntr = 0
      }
      
      # WHAT TO DO with number of new ISAs? -------------
      
    }
  )
  
}
