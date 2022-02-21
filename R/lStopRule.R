#' Check if Platform Stopping Rules are reached
#' 
#' Functions for creating, validating and simple use of class lStopRule
#' 
#' @param fnStopRule   Function which will check if Stop Rules are reached
#' @param lAddArgs     Further arguments used in fnStopRule
#' 
#' @examples
#' 
#' @name lStopRule
#' 
#' @export
#' @rdname lStopRule
# Constructor Function
new_lStopRule <- function(
  # Function that is used in checking if platform stopping rules are reached
  fnStopRule = function(
    lPltfTrial, # List of current platform trial status
    lAddArgs    # List of further arguments for this module
    ) {}, 
  # List of Arguments used with fnStopRule function
  lAddArgs = list()
  ) {
  structure(
    list(
      fnStopRule = fnStopRule,
      lAddArgs   = lAddArgs
     ),
    class        = "lStopRule"
  )
}
#' @export
#' @rdname lStopRule
# Validator Function
validate_lStopRule <- function(x) {
  
}
#' @export
#' @rdname lStopRule
# Helper Function
lStopRule <- function(nWeeks = NULL, bNoActive = NULL) {
  
  new_lStopRule(
    
    fnStopRule = function(lPltfTrial, lAddArgs) {
      
      if (!is.null(nWeeks)) {
        # Stop after x weeks
        ret <- lPltfTrial$lSnap$dCurrTime >= lAddArgs$nWeeks
      } else if (!is.null(bNoActive)) {
        # Stop if all have final decision
        ret <- all(!is.na(sapply(lPltfTrial$isa, function(x) x$cEndReason)))
      } else {
        stop("No valid stopping rule was specified.")
      }
      
      return(ret)
    },
    lAddArgs   = list(nWeeks = nWeeks, bNoEnrol = bNoActive)
  )

}
