#' Synthesize individual trial simulations into OCs
#' 
#' Functions and rules for synthesizing results of class lOCSynth
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lOCSynth
#' 
#' @export
#' @rdname lOCSynth
# Constructor Function
new_lOCSynth <- function(
    # Function that is used in updating between ISA allocation ratios
  fnOCSynth = function(
    lIndTrials, # List of individual trial results
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnOCSynth function
  lAddArgs = list()
) {
  structure(
    list(
      fnOCSynth     = fnOCSynth,
      lAddArgs      = lAddArgs
    ),
    class           = "lOCSynth"
  )
}
#' @export
#' @rdname lOCSynth
# Validator Function
validate_lOCSynth <- function(x) {
  
}
#' @export
#' @rdname lOCSynth
# Helper Function
# By default return NULL
lOCSynth <- function() {
  
  new_lOCSynth(
    fnOCSynth = function(lIndTrials, lAddArgs) {
      
      NULL
      
    },
    lAddArgs   = list()
  )
  
}
