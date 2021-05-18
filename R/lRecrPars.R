#' Recruitment Parameters
#' 
#' Functions for creating, validating and simple use of class lRecrPars
#' 
#' @param fRecrPars   Function
#' @param lArgs       Arguments
#' 
#' @examples
#' 
#' x <- lRecrPars(4)
#' validate_lRecrPars(x)
#' y <- list(
#'   fRecrProc = function(x) {x},
#'   lArgs = list()
#' )
#' validate_lRecrPars(y)
#' 
#' @name lRecrPars
#' 
#' @export
#' @rdname lRecrPars
# Constructor Function
new_lRecrPars <- function(
  # Function that is used in recruitment
  fRecrProc = function(
    dCurrTime, 
    dActvIntr, 
    lArgs
    ) {}, 
  # List of Arguments used with fRecrProc function
  lArgs = list()
  ) {
  structure(
    list(
      fRecrProc = fRecrProc,
      lArgs     = lArgs
     ),
    class       = "lRecrPars"
  )
}
#' @export
#' @rdname lRecrPars
# Validator Function
validate_lRecrPars <- function(x) {
  
  # Error if length of list < 2
  if (length(x) < 2) {
    warning(
      "Too few attributes were specified."
    )
  }
  
  # Check whether correct names
  if (names(x) != c("fRecrProc", "lArgs")) {
    stop(
      "Wrong names."
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
  if (!"dCurrTime" %in% names(f_args) | !"dActvIntr" %in% names(f_args) | !"lArgs" %in% names(f_args)) {
    stop(
      "Function not properly specified."
    )
  }
  
  # Warnings if length of list > 2
  if (length(x) > 2) {
    warning(
      "Too many attributes were specified; ignoring additional attributes."
    )
  }
  
}
#' @export
#' @rdname lRecrPars
# Helper Function
lRecrPars <- function(lambda) {
  
  new_lRecrPars(
    # In easy Version: Use simple Poisson Distribution per iteration
    fRecrProc = function(dCurrTime, dActvIntr, lArgs) {rpois(1, lambda = lArgs$lambda)},
    lArgs     = list(lambda = lambda)
  )

}
