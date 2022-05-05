#' Simulation of patient Baseline data
#' 
#' Functions and rules for simulating patient Baseline data of class lSimBase
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lSimBase
#' 
#' @export
#' @rdname lSimBase
# Constructor Function
new_lSimBase <- function(
  # Function that is used in updating between ISA allocation ratios
  fnSimBase = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnSimBase function
  lAddArgs = list()
) {
  structure(
    list(
      fnSimBase   = fnSimBase,
      lAddArgs    = lAddArgs
    ),
    class         = "lSimBase"
  )
}
#' @export
#' @rdname lSimBase
# Validator Function
validate_lSimBase <- function(x) {
  
}
#' @export
#' @rdname lSimBase
# Helper Function creates sex and age and current time
lSimBase <- 
  function(
    vars = list(
      list("time", "t")
    ),
    names = c("InclusionTime")
  ) {
  
  # By default, use genpat function
  new_lSimBase(
    fnSimBase = function(lPltfTrial, lAddArgs) {

      fnGenPatData(
        n = 1,
        vars = lAddArgs$vars,
        id = "no",
        cnames = lAddArgs$names,
        t = list(lPltfTrial$lSnap$dCurrTime, "time")
      )
      
    },
    
    lAddArgs   = list(vars = vars, names = names)
  )
  
}
