#' Between ISA allocation ratio
#' 
#' Functions and rules for between ISA allocation ratio of class lUpdIntrAlloc
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lUpdIntrAlloc
#' 
#' @export
#' @rdname lUpdIntrAlloc
# Constructor Function
new_lUpdIntrAlloc <- function(
  # Function that is used in updating between ISA allocation ratios
  fnUpdIntrAlloc = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnUpdIntrAlloc function
  lAddArgs = list()
) {
  structure(
    list(
      fnUpdIntrAlloc = fnUpdIntrAlloc,
      lAddArgs       = lAddArgs
    ),
    class            = "lUpdIntrAlloc"
  )
}
#' @export
#' @rdname lUpdIntrAlloc
# Validator Function
validate_lUpdIntrAlloc <- function(x) {
  
}
#' @export
#' @rdname lUpdIntrAlloc
# Helper Function
lUpdIntrAlloc <- function() {
  
  # By default, balances allocation ratio between ISAs
  new_lUpdIntrAlloc(
    fnUpdIntrAlloc = function(lPltfTrial, lAddArgs) {

      for (i in 1:length(lPltfTrial$isa)) {
        lPltfTrial$isa[[i]]$dAlloc <- 
          ifelse(
            lPltfTrial$isa[[i]]$bEnrol,
            1,
            0
          )
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
