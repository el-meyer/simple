#' Addition of ISAs into ongoing platform trial
#' 
#' Functions for creating, validating and simple use of class lAddIntr
#' 
#' @param fnAddIntr    Function which will add ISAs
#' @param lAddArgs     Further arguments used in fnAddIntr
#' 
#' @examples
#' 
#' @name lAddIntr
#' 
#' @export
#' @rdname lAddIntr
# Constructor Function
new_lAddIntr <- function(
  # Function that is used in adding ISAs to ongoing platform trial
  fnAddIntr = function(
    lPltfDsgn,  # List of platfom design parameters
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAddIntr function
  lAddArgs = list()
) {
  structure(
    list(
      fnAddIntr  = fnAddIntr,
      lAddArgs   = lAddArgs
    ),
    class        = "lAddIntr"
  )
}
#' @export
#' @rdname lAddIntr
# Validator Function
validate_lAddIntr <- function(x) {
  
}
#' @export
#' @rdname lAddIntr
# Helper Function
lAddIntr <- function(nTrt) {
  
  new_lAddIntr(
    # In easy Version: Simply run lInitIntr and append to list
    
    fnAddIntr = function(lPltfDsgn, lPltfTrial, lAddArgs) {
      
      # For every new ISA, run the respective lInitIntr function and add to lPltfTrial$isas
      for (i in 1:lAddArgs$nTrt) {
        
        lPltfTrial$isa <- 
          c(
            lPltfTrial$isa,
            list(
              do.call(
                match.fun(lPltfDsgn$lIntrDsgn[[length(lPltfTrial$isa) + i]]$lInitIntr$fnInitIntr),
                args = 
                  list(
                    lPltfTrial = lPltfTrial,
                    lAddArgs   = lPltfDsgn$lIntrDsgn[[length(lPltfTrial$isa) + i]]$lInitIntr$lAddArgs
                  )
              )
            )
          )
      }
      
      # Return modified object
      return(lPltfTrial)
      
    },
    
    lAddArgs   = list(nTrt = nTrt)
  )
  
}
