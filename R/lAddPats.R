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
  # Function that is used in updating between ISA allocation ratios
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
