#' List of current trial snapshot
#' 
#' List of snapshot variables to be extracted in simulations and passed to different methods
#' 
#' @param fnAddIntr    Function which will add ISAs
#' @param lAddArgs     Further arguments used in fnAddIntr
#' 
#' @examples
#' 
#' @name lSnap
#' 
#' @export
#' @rdname lSnap
# Constructor Function
new_lSnap <-  function(
  fnSnap = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnSnap function
  lAddArgs = list()
  ) {
    structure(
      list(
        fnSnap    = fnSnap,
        lAddArgs  = lAddArgs
      ),
      class       = "lSnap"
    )
}
#' @export
#' @rdname lSnap
# Validator Function
validate_lSnap <- function(x) {
  
  # Error if list is not of class lSnap
  if (class(x) != "lSnap") {
    stop(
      "Object is not of class lSnap."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnSnap", "lAddArgs"))) {
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
#' @rdname lSnap
# Helper Function
lSnap <- function() {
  
  new_lSnap(
    # In easy Version: Summarize lPltfTrial in a certain way
    fnSnap = function(lPltfTrial, lAddArgs) {
      
      lPltfTrial$lSnap <- 
        list(
          # current time
          dCurrTime       = lPltfTrial$lSnap$dCurrTime, 
          # current number of active cohorts (== enrolling)
          dActvIntr       = sum(sapply(lPltfTrial$isa, function(x) x$bEnrl)), 
          # number of outgoing ISAs at this time point
          dExitIntr       = 0,
          # vector of all ISA inclusion times so far
          vIntrInclTimes  = sapply(lPltfTrial$isa, function(x) x$nStartTime), 
          # vector of all ISA exit times (==end enrollment) so far
          vIntrExitTimes  = sapply(lPltfTrial$isa, function(x) x$nEndEnrlTime),
          # vector of all ISA decision times so far
          vIntrDecTimes   = sapply(lPltfTrial$isa, function(x) x$nEndTime),
          # vector of all ISA decisions so far
          vIntrDec        = sapply(lPltfTrial$isa, function(x) x$cEndReason),
          # Vector of current allocation ratio between ISAs
          vCurrAllocRatio = sapply(lPltfTrial$isa, function(x) x$dAlloc), 
          # Overall number of analyses already conducted
          nAnalysisOver   = sum(sapply(lPltfTrial$isa, function(x) length(x$lAnalyses)), na.rm = TRUE) 
        )
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}

#' @export
#' @rdname lSnap
# Summary Function
summary.lSnap <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnSnap)))[2]
  
  cat("Specified accrual function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lAddArgs)
}

