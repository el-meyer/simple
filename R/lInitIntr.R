#' Initialization of ISAs Dataset
#' 
#' Functions for creating, validating and simple use of class lInitIntr
#' 
#' @param fnInitTrt    Function which will initiate the dataframe of ISAs
#' @param lAddArgs     Further arguments used in fnInitTrt
#' 
#' @examples
#' 
#' @name lInitIntr
#' 
#' @export
#' @rdname lInitIntr
# Constructor Function
new_lInitIntr <- function(
  # Function that is used in initializing of ISA data frame
  fnInitTrt = function(
    lPltfDsgn, # List of platform design parameters
    lAddArgs    # List of further arguments for this module
    ) {}, 
  # List of Arguments used with fnInitTrt function
  lAddArgs = list()
  ) {
  structure(
    list(
      fnInitTrt  = fnInitTrt,
      lAddArgs   = lAddArgs
     ),
    class        = "lInitIntr"
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
lInitIntr <- function(nTrt) {
  
  new_lInitIntr(
    # In easy Version: Simple start with nTrt ISAs and balanced allocation ratio
    
    fnInitTrt = function(lPltfDsgn, lAddArgs) {
    
    isas <- list()
    
    for (i in 1:nTrt) {
      
      isas[[i]] <- 
        list(
          id           = i, # assign IDs accoring to how many ISAs start
          name         = lPltfDsgn$lIntrDsgn[[i]]$name, # get the first names
          n_cur        = 0, # Current sample size allocated to this ISA
          enrol        = TRUE, # is ISA currently enrolling
          alloc        = 1, # allocation ratio relative to other ISAs
          start_time   = 0, # at what calendar time was ISA started
          nAnalysis    = 0, # number of analyses conducted for this ISA
          end_time     = NA, # at what calendar time was ISA stopped
          end_reason   = NA # reason why ISA was stopped
        )
      
    }
      
      return(isas)
      
    },
    
    lAddArgs   = list(nTrt = nTrt)
  )

}
