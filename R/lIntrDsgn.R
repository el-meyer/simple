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
  ...
) {
  structure(
    list(
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
  name,
  lTrtEffect,
  
) {
  
  new_lIntrDsgn(
    name,
    lTrtEffect
  )
  
}
