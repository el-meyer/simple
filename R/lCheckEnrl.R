#' Check ISA Enrllment
#' 
#' Functions and rules for within ISA checking of Enrllment of class lCheckEnrl
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lCheckEnrl
#' 
#' @export
#' @rdname lCheckEnrl
# Constructor Function
new_lCheckEnrl <- function(
  # Function that is used in checking ISA Enrllment
  fnCheckEnrl = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnCheckEnrl function
  lAddArgs = list()
) {
  structure(
    list(
      fnCheckEnrl  = fnCheckEnrl,
      lAddArgs      = lAddArgs
    ),
    class           = "lCheckEnrl"
  )
}
#' @export
#' @rdname lCheckEnrl
# Validator Function
validate_lCheckEnrl <- function(x) {
  
}
#' @export
#' @rdname lCheckEnrl
# Helper Function
lCheckEnrl <- function() {
  
  new_lCheckEnrl(
    fnCheckEnrl = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      
      # By default just check if final sample size was reached
      
      if (
        lPltfTrial$isa[[lAddArgs$current_id]]$nMaxNIntr <= 
        nrow(do.call(rbind.data.frame, lPltfTrial$isa[[lAddArgs$current_id]]$lPats))
        ) {
        
        if (lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl) {
          print(
            paste0(
              "ISA ",
              lAddArgs$current_id,
              " has stopped enrollment at time ",
              lPltfTrial$lSnap$dCurrTime
            )
          )
        }
        
        lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl <- FALSE
        lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime <- lPltfTrial$lSnap$dCurrTime
        lPltfTrial$lSnap$dExitIntr <- lPltfTrial$lSnap$dExitIntr + 1
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
