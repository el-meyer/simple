#' Platform Trial Design List
#' 
#' Platform Trial Design List of class lPltfDsgn
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lPltfDsgn
#' 
#' @export
#' @rdname lPltfDsgn
# Constructor Function
new_lPltfDsgn <- function(
  lFnDef        = list(),
  lIntrDsgn     = lIntrDsgn,
  ...
) {
  structure(
    list(
      lFnDef    = lFnDef,
      lIntrDsgn = lIntrDsgn,
      ...
    ),
    class       = "lPltfDsgn"
  )
}
#' @export
#' @rdname lPltfDsgn
# Validator Function
validate_lPltfDsgn <- function(x) {
  
}
#' @export
#' @rdname lPltfDsgn
# Helper Function creates ISAs with binary endpoints
lPltfDsgn <- function(
  lIntrDsgn,
  ...
) {
  
  new_lPltfDsgn(
    lFnDef    = lFnDef(),
    lIntrDsgn = lIntrDsgn,
    ...
  )
  
}

