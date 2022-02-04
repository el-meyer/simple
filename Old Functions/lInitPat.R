#' Initialization of Treatments Dataset
#' 
#' Functions for creating, validating and simple use of class lInitTrt
#' 
#' @param fnInitTrt    Function which will initiate the dataframe of treatments
#' @param lAddArgs     Further arguments used in fnInitTrt
#' 
#' @examples
#' 
#' @name lInitTrt
#' 
#' @export
#' @rdname lInitPat
# Constructor Function
new_lInitPat <- function(
  # Function that is used in initializing of treatment data frame
  fnInitPat = function(
    lPltfDsgn, # List of platform design parameters
    lAddArgs    # List of further arguments for this module
    ) {}, 
  # List of Arguments used with fnInitPat function
  lAddArgs = list()
  ) {
  structure(
    list(
      fnInitPat  = fnInitPat,
      lAddArgs   = lAddArgs
     ),
    class        = "lInitPat"
  )
}
#' @export
#' @rdname lInitPat
# Validator Function
validate_lInitPat <- function(x) {
  
}
#' @export
#' @rdname lInitPat
# Helper Function
# By default, patient dataset is empty and column names are specified
lInitPat <- function(names) {
  
  new_lInitPat(
    fnInitPat = function(lPltfDsgn, lAddArgs) {
      patients <- data.frame(matrix(nrow = 0, ncol = length(lAddArgs$names)))
      colnames(patients) <- lAddArgs$names
      return(patients)
    },
    lAddArgs = list(names = names)
  )

}
