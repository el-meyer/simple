#' ISA Design List
#' 
#' Different ISA Designs to draw from of class lIntrDsgnList
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lIntrDsgn
#' 
#' @export
#' @rdname lIntrDsgn
# Constructor Function
new_lIntrDsgn <- function(
  lInitIntr       = list(),
  lAllocArm       = list(),
  lPatOutcome     = list(),
  lCheckAnlsMstn  = list(),
  lAnls           = list(),
  lSynthRes       = list(),
  lCheckEnrl      = list(),
  ...
) {
  structure(
    list(
      lInitIntr       = lInitIntr,
      lAllocArm       = lAllocArm,
      lPatOutcome     = lPatOutcome,
      lCheckAnlsMstn  = lCheckAnlsMstn,
      lAnls           = lAnls,
      lSynthRes       = lSynthRes,
      lCheckEnrl      = lCheckEnrl,
      ...
    ),
    class       = "lIntrDsgn"
  )
}
#' @export
#' @rdname lIntrDsgn
# Validator Function
validate_lIntrDsgn <- function(x) {
  
}
#' @export
#' @rdname lIntrDsgn
# Helper Function creates ISAs with binary endpoints
lIntrDsgn <- function(
  lInitIntr       ,
  lAllocArm       ,
  lPatOutcome     ,
  lCheckAnlsMstn  ,
  lAnls           ,
  lSynthRes       ,
  lCheckEnrl      ,
  ...
) {

  new_lIntrDsgn(
    lInitIntr       ,
    lAllocArm       ,
    lPatOutcome     ,
    lCheckAnlsMstn  ,
    lAnls           ,
    lSynthRes       ,
    lCheckEnrl      ,
    ...
  )
  
}
