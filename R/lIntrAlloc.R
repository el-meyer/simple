#' Between ISA allocation ratio
#' 
#' Functions and rules for between ISA allocation ratio of class lIntrAlloc
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lIntrAlloc
#' 
#' @export
#' @rdname lIntrAlloc
# Constructor Function
new_lIntrAlloc <- function(
  # Function that is used in updating between ISA allocation ratios
  fnIntrAlloc = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnIntrAlloc function
  lAddArgs = list()
) {
  structure(
    list(
      fnIntrAlloc = fnIntrAlloc,
      lAddArgs    = lAddArgs
    ),
    class         = "lIntrAlloc"
  )
}
#' @export
#' @rdname lIntrAlloc
# Validator Function
validate_lIntrAlloc <- function(x) {
  
}
#' @export
#' @rdname lIntrAlloc
# Helper Function
lIntrAlloc <- function() {
  
  # By default, balances allocation ratio between ISAs
  new_lIntrAlloc(
    fnIntrAlloc = function(lPltfTrial, lAddArgs) {

      # give all "active" cohorts a "1" and all others "0"
      lPltfTrial$treatments$alloc[lPltfTrial$treatments$active] <- 1
      lPltfTrial$treatments$alloc[!lPltfTrial$treatments$active] <- 0
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
