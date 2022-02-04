#' Between ISA allocation ratio
#' 
#' Functions and rules for between ISA allocation ratio of class lIntrAlloc
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lIntrAlloc
#' 
#' @export
#' @rdname lIntrAlloc
# Constructor Function
new_lIntrAlloc <- function(
  # Function that is used in updating between ISA allocation ratios
  fnIntrAlloc = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnIntrAlloc function
  lAddArgs = list()
) {
  structure(
    list(
      fnIntrAlloc  = fnIntrAlloc,
      lAddArgs    = lAddArgs
    ),
    class         = "lIntrAlloc"
  )
}
#' @export
#' @rdname lIntrAlloc
# Validator Function
validate_lIntrAlloc <- function(x) {
  
}
#' @export
#' @rdname lIntrAlloc
# Helper Function
lIntrAlloc <- function() {
  
  # By default, just randomize according to weights in lPltfTrial$isa
  new_lIntrAlloc(
    fnIntrAlloc = function(lPltfTrial, lAddArgs) {
      
      # contains numbers (including 0)
      alloc_ratio <- sapply(lPltfTrial$isa, function(x) x$dAlloc)
      
      # What to do if all dAlloc == 0?
      if (all(alloc_ratio == 0)) {
        
        print("Patients were not allocated to ISAs due to no valid allocation ratios.")
        
      } else {
        
        # Assign ID not Name (for sure unique)
        lPltfTrial$lSnap$newdat_df$ISA <- 
          sample(
            x        = sapply(lPltfTrial$isa, function(x) x$nID), 
            size     = nrow(lPltfTrial$lSnap$newdat_df),
            replace  = TRUE, 
            prob     = alloc_ratio/sum(alloc_ratio)
          )
        
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
