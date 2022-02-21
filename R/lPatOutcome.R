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
lPatOutcome <- function(cGroups, dRates, dTrend, nLag) {
  
  new_lPatOutcome(
    fnPatOutcome = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
        
      for (i in 1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)) {
        
        # Get the arm of the patient
        index <- 
          which(lAddArgs$cGroups == lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Arm[i])
        
        # Get success probability
        prob <- 
          max(
            min(
              lAddArgs$dRates[index] + lAddArgs$dTrend * lPltfTrial$lSnap$dCurrTime,
              1
            ),
            0
          )
        
        lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Event[i] <- 
          rbinom(1, 1, prob)
        
        lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- 
          lPltfTrial$lSnap$dCurrTime + lAddArgs$nLag
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list(cGroups = cGroups, dRates = dRates, dTrend = dTrend, nLag = nLag)
  )
  
}
