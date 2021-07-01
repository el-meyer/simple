#' ISA inclusion rules
#' 
#' Rules for ISA starting times of class lPltfStopRule
#' 
#' @param bIntrRepl       Boolean - should exiting ISAs be replaced
#' @param nMaxIntr        Maximum number of ISAs that can enter the platform over time
#' @param nMaxAdd         Maximum number of ISAs that can enter the platform at any timepoint
#' @param fnAddNewIntr    Function that checks how many new ISAs should be added to the platform
#' @param lArgs           Further arguments used in fnAddNewIntr
#' 
#' @examples
#' 
#' @name lIntrIncl
#' 
#' @export
#' @rdname lIntrIncl
# Constructor Function
new_lIntrIncl <- 
  function(
    bIntrRepl    = NULL,  
    nMaxIntr     = NULL, 
    nMaxAdd      = NULL,
    fnAddNewIntr = function(
      dCurrTime,
      dLastAddTime,
      nPatsPostAdd
    ) {},
    lArgs        = list()
  ) {
    structure(
      list(
        bIntrRepl     = bIntrRepl,
        nMaxIntr      = nMaxIntr,
        nMaxAdd       = nMaxAdd,
        fnAddNewIntr  = fnAddNewIntr,
        lArgs         = lArgs
      ),
      class       = "lIntrIncl"
    )
  }
#' @export
#' @rdname lIntrIncl
# Validator Function
validate_lIntrIncl <- function(x) {
  
}
#' @export
#' @rdname lIntrIncl
# Helper Function
lIntrIncl <- function(dRandAddProb) {
  # In simple version: 
  new_lIntrIncl(
    bIntrRepl     = TRUE, # Replace exiting ISAs
    nMaxIntr      = 10,   # Add maximum 10 ISAs during course of trial
    nMaxAdd       = 2,    # Add maximum two ISAs at the same time
    fnAddNewIntr  = function(dCurrTime, dLastAddTime, nPatsPostAdd, lArgs) {
      return(sample(0:1, prob = c(1 - lArgs$dRandAddProb, lArgs$dRandAddProb)))
    },                    # Add ISAs with a certain probability in every time unit
    lArgs         = list(dRandAddProb = dRandAddProb)
  )
  
}
