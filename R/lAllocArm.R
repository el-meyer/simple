#' Within ISA allocation ratio
#' 
#' Functions and rules for within ISA allocation ratio of class lAllocArm
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lAllocArm
#' 
#' @export
#' @rdname lAllocArm
# Constructor Function
new_lAllocArm <- function(
  # Function that is used in updating between ISA allocation ratios
  fnAllocArm = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAllocArm function
  lAddArgs = list()
) {
  structure(
    list(
      fnAllocArm  = fnAllocArm,
      lAddArgs    = lAddArgs
    ),
    class         = "lAllocArm"
  )
}
#' @export
#' @rdname lAllocArm
# Validator Function
validate_lAllocArm <- function(x) {
  
}
#' @export
#' @rdname lAllocArm
# Helper Function
lAllocArm <- function() {
  
  new_lAllocArm(
    fnAllocArm = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
