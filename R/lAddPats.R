#' Add simulated Pats to ISAs
#' 
#' Functions and rules for adding Pats to ISAs of class lAddPats
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lAddPats
#' 
#' @export
#' @rdname lAddPats
# Constructor Function
new_lAddPats <- function(
  # Function that is used in adding patients to the respective ISAs
  fnAddPats = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAddPats function
  lAddArgs = list()
) {
  structure(
    list(
      fnAddPats  = fnAddPats,
      lAddArgs   = lAddArgs
    ),
    class        = "lAddPats"
  )
}
#' @export
#' @rdname lAddPats
# Validator Function
validate_lAddPats <- function(x) {
  
  # Error if list is not of class lAddPats
  if (class(x) != "lAddPats") {
    stop(
      "Object is not of class lAddPats."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnAddPats", "lAddArgs"))) {
    stop(
      "Wrong module attributes (too many, too few or wrong names)."
    )
  }
  
  # Errors if first element not function
  if (!is.function(x[[1]])) {
    stop(
      "First element is not a function."
    )
  }
  
  # Error if second element not a list
  if (!is.list(x[[2]])) {
    stop(
      "Second element is not a list."
    )
  }
  
  f <- match.fun(x[[1]])
  f_args <- as.list(args(f))
  
  # Check input parameters of function
  if (!"lPltfTrial" %in% names(f_args) | !"lAddArgs" %in% names(f_args)) {
    stop(
      "Function not properly specified."
    )
  }
  
}
#' @export
#' @rdname lAddPats
# Helper Function
lAddPats <- function() {
  
  # By default, just add Pats to corresponding list
  new_lAddPats(
    fnAddPats = function(lPltfTrial, lAddArgs) {
      
      # Only those ISAs that actually received Pats this timestep
      for (i in unique(lPltfTrial$lSnap$newdat_df$ISA)) {
        
        lPltfTrial$isa[[i]]$lPats <-
          c(
            lPltfTrial$isa[[i]]$lPats,
            list(subset(lPltfTrial$lSnap$newdat_df, ISA == i))
          )
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
