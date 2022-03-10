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
    # In easy Version: Simple start with nTrt ISAs
    
    fnInitTrt = function(lPltfDsgn, lAddArgs) {
    
    isas <- list()
    
    for (i in 1:lAddArgs$nTrt) {
      
      isas[[i]] <- 
        do.call(
          match.fun(lPltfDsgn$lIntrDsgn[[i]]$lInit$fnInit),
          args = 
            list(
              lAddArgs = list(
                id = i,
                name = lPltfDsgn$lIntrDsgn[[i]]$name,
                time = 0
              )
            )
        )
    }
      
      return(isas)
      
    },
    
    lAddArgs   = list(nTrt = nTrt)
  )

}
