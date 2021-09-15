#' List of global variables
#' 
#' List of global variables to be extracted in simultions and passed to different methods
#' 
#' @param lVars           List of global variables
#' 
#' @examples
#' 
#' x <- 
#'   lGlobVars(
#'     dCurrTime      = 10, # current platform time
#'     dActvIntr      = 2, # number of ISAs active at beginning of current platform time
#'     dPatsPostAdd   = 20, # number of patients added since last time ISA was added
#'     dExitIntr      = 1, # number of outgoing ISAs at current platform time
#'     vIntrInclTimes = c(1, 9), # vector of all ISA inclusion times so far
#'     vIntrExitTimes = c(10) # vector of all ISA exit times so far
#'    )
#' validate_lGlobVars(x)
#' summary(x)
#' 
#' @name lGlobVars
#' 
#' @export
#' @rdname lGlobVars
# Constructor Function
new_lGlobVars <- 
  function(
    lVars         = list()
  ) {
    structure(
      list(
        lVars     = lVars
      ),
      class       = "lGlobVars"
    )
  }
#' @export
#' @rdname lGlobVars
# Validator Function
validate_lGlobVars <- function(x) {
  
  # Error if length of list < 1
  if (length(x) < 1) {
    warning(
      "Too few attributes were specified."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("lVars"))) {
    stop(
      "Wrong names."
    )
  }
  
  # Error if first element not a list
  if (!is.list(x[[1]])) {
    stop(
      "First element is not a list."
    )
  }
  
  # Warnings if length of list > 1
  if (length(x) > 1) {
    warning(
      "Too many attributes were specified; ignoring additional attributes."
    )
  }
  
}
#' @export
#' @rdname lGlobVars
# Helper Function
lGlobVars <- 
  function(...) {
  
  # Default list of global variables
    structure(
      list(
        lVars = list(...)
      ),
      class       = "lGlobVars"
    )
  
}

#' @export
#' @rdname lGlobVars
# Summary Function
summary.lGlobVars <- function(x, ...) {
  
  cat("\n Specified global variables: \n")
  print(x$lVars, ...)
  
}
