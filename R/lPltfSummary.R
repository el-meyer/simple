#' Summarize the platform trial
#' 
#' Functions for summarizing the platform trial of class lPltfSummary
#' 
#' @param fnPltfSummary   Function which will check if Stop Rules are reached
#' @param lAddArgs        Further arguments used in fnPltfSummary
#' 
#' @examples
#' 
#' @name lPltfSummary
#' 
#' @export
#' @rdname lPltfSummary
# Constructor Function
new_lPltfSummary <- function(
  # Function that is used in checking if platform stopping rules are reached
  fnPltfSummary = function(
    lPltfTrial, # List of current platform trial status
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnPltfSummary function
  lAddArgs = list()
) {
  structure(
    list(
      fnPltfSummary = fnPltfSummary,
      lAddArgs      = lAddArgs
    ),
    class           = "lPltfSummary"
  )
}
#' @export
#' @rdname lPltfSummary
# Validator Function
validate_lPltfSummary <- function(x) {
  
}
#' @export
#' @rdname lPltfSummary
# Helper Function
lPltfSummary <- function() {
  
  new_lPltfSummary(
    
    fnPltfSummary = function(lPltfTrial, lAddArgs) {
      
      lPltfTrial$isa
      
    },
    lAddArgs   = list()
  )
  
}
