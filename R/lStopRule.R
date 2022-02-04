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
lStopRule <- function() {
  
  new_lStopRule(
    # # In easy Version: Stop platform, if all treatments have final decision!!
    # fnStopRule = function(lPltfTrial, lAddArgs) {
    #   all(!is.na(sapply(lPltfTrial$isa, function(x) x$cEndReason)))
    # Stop After 50 weeks
    fnStopRule = function(lPltfTrial, lAddArgs) {
      lPltfTrial$lSnap$dCurrTime >= 50
    },
    lAddArgs   = list()
  )

}
