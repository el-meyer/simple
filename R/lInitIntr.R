#' Initialization of ISA
#' 
#' Functions for creating, validating and simple use of class lInitIntr
#' 
#' @param fnInitIntr    Function which will initiate the ISA
#' @param lAddArgs  Further arguments used in fnInitIntr
#' 
#' @examples
#' 
#' @name lInitIntr
#' 
#' @export
#' @rdname lInitIntr
# Constructor Function
new_lInitIntr <- function(
  # Function that is used in initializing of ISA
  fnInitIntr = function(
    lPltfTrial,  # List of current platform trial status
    lAddArgs     # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnInitIntr function
  lAddArgs = list()
) {
  structure(
    list(
      fnInitIntr     = fnInitIntr,
      lAddArgs       = lAddArgs
    ),
    class            = "lInitIntr"
  )
}
#' @export
#' @rdname lInitIntr
# Validator Function
validate_lInitIntr <- function(x) {
  
}
#' @export
#' @rdname lInitIntr
# Helper Function
lInitIntr <- function(name) {
  
  new_lInitIntr(
    # In easy Version: Simple start with balanced allocation ratio
    
    fnInitIntr = function(lPltfTrial, lAddArgs) {
        
      list(
        # assign ID depending on how many ISAs are already in platform
        nID          = length(lPltfTrial$isa) + 1, 
        # get the name
        cName        = lAddArgs$name, 
        # Current sample size allocated to this ISA
        nPats        = 0, 
        # is ISA currently enrolling
        bEnrol       = TRUE, 
        # allocation ratio relative to other ISAs
        dAlloc       = 1, 
        # at what calendar time was ISA started
        nStartTime   = lPltfTrial$lSnap$dCurrTime, 
        # number of analyses conducted for this ISA
        nAnalysis    = 0, 
        # at what calendar time was ISA stopped
        nEndTime     = NA, 
        # reason why ISA was stopped
        cEndReason   = NA,
        # List of conducted analyses
        lAnalyses    = list(),
        # List of patients in this ISA
        lPats        = list()
      )
      
    },
    
    lAddArgs   = list(name = name)
  )
  
}
