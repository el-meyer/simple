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
  
  # Error if list is not of class lStopRule
  if (class(x) != "lStopRule") {
    stop(
      "Object is not of class lStopRule."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnStopRule", "lAddArgs"))) {
    stop(
      "Wrong module attributes (too many, too few or wrong names)."
    )
  }
  
  # Errors if first element not function
  if (!is.function(x[[1]])) {
    stop(
      "First element is not a function."
    )
  }
  
  # Error if second element not a list
  if (!is.list(x[[2]])) {
    stop(
      "Second element is not a list."
    )
  }
  
  f <- match.fun(x[[1]])
  f_args <- as.list(args(f))
  
  # Check input parameters of function
  if (!"lPltfTrial" %in% names(f_args) | !"lAddArgs" %in% names(f_args)) {
    stop(
      "Function not properly specified."
    )
  }
  
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
