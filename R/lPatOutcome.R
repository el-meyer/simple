#' Simulate patient outcomes
#' 
#' Functions and rules for simulation of patient outcomes of class lPatOutcome
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lPatOutcome
#' 
#' @export
#' @rdname lPatOutcome
# Constructor Function
new_lPatOutcome <- function(
  # Function that is used in simulating patient outcomes
  fnPatOutcome = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnPatOutcome function
  lAddArgs = list()
) {
  structure(
    list(
      fnPatOutcome  = fnPatOutcome,
      lAddArgs      = lAddArgs
    ),
    class           = "lPatOutcome"
  )
}
#' @export
#' @rdname lPatOutcome
# Validator Function
validate_lPatOutcome <- function(x) {
  
}
#' @export
#' @rdname lPatOutcome
# Helper Function
# By default, binary outcome with response rate specifiable and time trend 
# or metric endpoint with mean and sd and mean trend over time
# Depnding on whether or not dSigma is specified, use binary
# or continuous endpoints
lPatOutcome <- function(
  cGroups, # Group Names to which the Thetas apply
  dTheta, # Location Parameters for cGroups
  dSigma = NULL, # Possible SD if continuous endpoint
  dTrend, # Trend per time unit for dTheta
  nLag # Time lag after which outcome is observed
) {
  
  new_lPatOutcome(
    fnPatOutcome = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
        
      for (i in 1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)) {
        
        # Get the arm of the patient
        index <- 
          which(lAddArgs$cGroups == lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Arm[i])
        
        # If patient was not assigned to an arm, the outcome is NA
        # Otherwise do regular simulations
        
        if (length(index) == 0) {
          
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome[i] <- NA
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- NA
          
        } else {
          
          # See whether continuous or binary endpoint
          if (!is.null(lAddArgs$dSigma)) {
            
            # Get mean
            thet <- 
              lAddArgs$dTheta[index] + lAddArgs$dTrend * lPltfTrial$lSnap$dCurrTime
            
            # Simulate outcome
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome[i] <- 
              rnorm(1, mean = thet, sd = lAddArgs$dSigma[index])
            
          } else {
            
            # Get success probability
            prob <- 
              max(
                min(
                  lAddArgs$dTheta[index] + lAddArgs$dTrend * lPltfTrial$lSnap$dCurrTime,
                  1
                ),
                0
              )
            
            # Simulate Outcome
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome[i] <- 
              rbinom(1, 1, prob)
            
          }
          
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- 
            lPltfTrial$lSnap$dCurrTime + lAddArgs$nLag
          
        }
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list(cGroups = cGroups, dTheta = dTheta, dSigma = dSigma, dTrend = dTrend, nLag = nLag)
  )
  
}
