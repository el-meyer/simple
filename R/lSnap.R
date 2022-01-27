#' List of current trial snapshot
#' 
#' List of snapshot variables to be extracted in simulations and passed to different methods
#' 
#' @param lVars           List of variables
#' 
#' @examples
#' 
#' x <- 
#'   lSnap(
#'     dCurrTime      = 10, # current platform time
#'     dActvIntr      = 2, # number of ISAs active at beginning of current platform time
#'     dExitIntr      = 1, # number of outgoing ISAs at current platform time
#'     vIntrInclTimes = c(1, 9), # vector of all ISA inclusion times so far
#'     vIntrExitTimes = c(10) # vector of all ISA exit times so far
#'    )
#' validate_lSnap(x)
#' summary(x)
#' 
#' @name lSnap
#' 
#' @export
#' @rdname lSnap
# Constructor Function
new_lSnap <- 
  function(
    ...
  ) {
    structure(
      list(
        ...
      ),
      class       = "lSnap"
    )
  }
#' @export
#' @rdname lSnap
# Validator Function
validate_lSnap <- function(x) {
  
  # Error if not a list
  if (!is.list(x)) {
    stop(
      "Element is not a list."
    )
  }
  
}
#' @export
#' @rdname lSnap
# Helper Function
lSnap <- 
  function(...) {
  
  # Default list of global variables
    structure(
      list(
        ...
      ),
      class       = "lSnap"
    )
  
}

#' @export
#' @rdname lSnap
# Summary Function
summary.lSnap <- function(x, ...) {
  
  cat("\n Specified snapshot variables: \n")
  print(x, ...)
  
}
