#' Synthesize trial results into decisions
#' 
#' Functions and rules for synthesizing results of class lSynthRes
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lSynthRes
#' 
#' @export
#' @rdname lSynthRes
# Constructor Function
new_lSynthRes <- function(
  # Function that is used in updating between ISA allocation ratios
  fnSynthRes = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnSynthRes function
  lAddArgs = list()
) {
  structure(
    list(
      fnSynthRes    = fnSynthRes,
      lAddArgs      = lAddArgs
    ),
    class           = "lSynthRes"
  )
}
#' @export
#' @rdname lSynthRes
# Validator Function
validate_lSynthRes <- function(x) {
  
}
#' @export
#' @rdname lSynthRes
# Helper Function
# By default just check p-value and make a decision
lSynthRes <- function(alpha = 0.05) {
  
  new_lSynthRes(
    fnSynthRes = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      if (length(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses) == 1 & is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
       
        if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[1]]$results < lAddArgs$alpha) {
          lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Efficacy"
        } else {
          lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility"
        }
        
        lPltfTrial$isa[[lAddArgs$current_id]]$nEndTime <- lPltfTrial$lSnap$dCurrTime
         
        print(
          paste0(
            "For ISA ",
            lAddArgs$current_id,
            " ",
            lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason,
            " has been declared at time ",
            lPltfTrial$lSnap$dCurrTime
          )
        )
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list(alpha = alpha)
  )
  
}
