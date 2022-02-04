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
          dActvIntr       = sum(sapply(lPltfTrial$isa, function(x) x$bEnrol)), 
          # number of outgoing ISAs at this time point
          dExitIntr       = 
            sum(
              sapply(
                lPltfTrial$isa, 
                function(x) x$nEndTime == lPltfTrial$lSnap$dCurrTime
              ),
              na.rm = TRUE
            ),
          # vector of all ISA inclusion times so far
          vIntrInclTimes  = sapply(lPltfTrial$isa, function(x) x$nStartTime), 
          # vector of all ISA exit times so far
          vIntrExitTimes  = sapply(lPltfTrial$isa, function(x) x$nEndTime),
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
