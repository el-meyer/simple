#' ISA inclusion rules
#' 
#' Rules for ISA starting times of class lPltfStopRule
#' 
#' @param bComnCont       Boolean - Is the first of the ISAs on the list a common control arm
#' @param vTime           Vector of starting times if starting times are fixed
#' @param bIntrRepl       Boolean - should exiting ISAs be replaced
#' @param dRandIntrIncl   Probability for random inclusion of new ISAs per time unit
#' @param nMaxIntr        Maximum number of ISAs that can enter the platform over time
#' @param nStrtIntr       Number of ISAs that are included at platform trial initiation
#' @param nPatsPreIntr    Number of Patients that need to be enrolled after a new ISA was included before a new ISA can be included
#' @param nTimePreIntr    Number of time-iterations after a new ISA was included before a new ISA can be included
#' 
#' @examples
#' 
#' @name lIntrStrtTime
#' 
#' @export
#' @rdname lIntrStrtTime
# Constructor Function
new_lIntrStrtTime <- 
  function(
    bComnCont,
    vTime, 
    bIntrRepl, 
    dRandIntrIncl, 
    nMaxIntr, 
    nStrtIntr, 
    nPatsPreIntr,
    nTimePreIntr
  ) {
    structure(
      list(
        bComnCont     = bComnCont,
        vTime         = vTime,
        bIntrRepl     = bIntrRepl,
        dRandIntrIncl = dRandIntrIncl,
        nMaxIntr      = nMaxIntr,
        nStrtIntr     = nStrtIntr,
        nPatsPreIntr  = nPatsPreIntr,
        nTimePreIntr  = nTimePreIntr
      ),
      class       = "lIntrStrtTime"
    )
  }
#' @export
#' @rdname lIntrStrtTime
# Validator Function
validate_lIntrStrtTime <- function(x) {
  
}
#' @export
#' @rdname lIntrStrtTime
# Helper Function
lIntrStrtTime <- function() {
  
  new_lIntrStrtTime(
    bComnCont     = TRUE,
    vTime         = NULL,
    bIntrRepl     = FALSE,
    dRandIntrIncl = 0.01,
    nMaxIntr      = 10,
    nStrtIntr     = 1,
    nPatsPreIntr  = 0,
    nTimePreIntr  = 0
  )
  
}
