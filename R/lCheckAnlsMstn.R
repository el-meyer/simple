#' Check Analysis Milestone
#' 
#' Functions and rules for checking analysis milestone of class lCheckAnlsMstn
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lCheckAnlsMstn
#' 
#' @export
#' @rdname lCheckAnlsMstn
# Constructor Function
new_lCheckAnlsMstn <- function(
  # Function that is used in updating between ISA allocation ratios
  fnCheckAnlsMstn = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnCheckAnlsMstn function
  lAddArgs = list()
) {
  structure(
    list(
      fnCheckAnlsMstn  = fnCheckAnlsMstn,
      lAddArgs         = lAddArgs
    ),
    class              = "lCheckAnlsMstn"
  )
}
#' @export
#' @rdname lCheckAnlsMstn
# Validator Function
validate_lCheckAnlsMstn <- function(x) {
  
}
#' @export
#' @rdname lCheckAnlsMstn
# Helper Function
lCheckAnlsMstn <- function(bInclude = TRUE, vInfTimes = c(1), column = "OutObsTime") {
  
  # Make sure vInfTimes is strictly monotonously increasing and in the range of 0 to 1
  
  if (length(vInfTimes) > 1) {
    
    if (!all(diff(vInfTimes) > 0)) {
      
      stop("Vector vInfTimes is not strictly monotonously increasing.")
      
    }
   
    if (max(vInfTimes) > 1 ) {
      stop("Last Analysis Time Point is > 1.")
    }
    
    if (min(vInfTimes) < 0 ) {
      stop("First Analysis Time Point is < 0.")
    }
     
  } else {
    
    stop("A single analysis time point != 1 was chosen.")
    
  }
  
  
  new_lCheckAnlsMstn(
    fnCheckAnlsMstn = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      
      # Should this ISA even be considered
      if (lAddArgs$bInclude) {
      
      # DF of Patients
      df <- 
        do.call(
          rbind.data.frame, 
          lPltfTrial$isa[[lAddArgs$current_id]]$lPats
        )
      
      # Number of patients with observed outcome
      nPatOutObs <-
        sum(df[[lAddArgs$column]] <= lPltfTrial$lSnap$dCurrTime, na.rm = TRUE)
      
      # Vector of TRUE or FALSE if percentage of events have been observed
      lPltfTrial$lSnap$isa_temp[[lAddArgs$current_id]]$AnlsMstn <- 
        (nPatOutObs / lPltfTrial$isa[[lAddArgs$current_id]]$nMaxNIntr) >= lAddArgs$vInfTimes
      
      } else {
        
        lPltfTrial$lSnap$isa_temp[[lAddArgs$current_id]]$AnlsMstn <- NULL
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list(bInclude = bInclude, vInfTimes = vInfTimes, column = column)
  )
  
}
