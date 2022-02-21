#' Between ISA allocation ratio
#' 
#' Functions and rules for between ISA allocation ratio of class lAllocIntr
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lAllocIntr
#' 
#' @export
#' @rdname lAllocIntr
# Constructor Function
new_lAllocIntr <- function(
  # Function that is used in updating between ISA allocation ratios
  fnAllocIntr = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAllocIntr function
  lAddArgs = list()
) {
  structure(
    list(
      fnAllocIntr  = fnAllocIntr,
      lAddArgs     = lAddArgs
    ),
    class          = "lAllocIntr"
  )
}
#' @export
#' @rdname lAllocIntr
# Validator Function
validate_lAllocIntr <- function(x) {
  
}
#' @export
#' @rdname lAllocIntr
# Helper Function
lAllocIntr <- function() {
  
  # By default, just randomize according to weights in lPltfTrial$isa
  new_lAllocIntr(
    fnAllocIntr = function(lPltfTrial, lAddArgs) {
      
      # contains numbers (including 0)
      alloc_ratio <- sapply(lPltfTrial$lSnap$isa_temp, function(x) x$dAlloc)
      
      # What to do if all dAlloc == 0?
      if (all(alloc_ratio == 0)) {
        
        print(
          paste0(
            "Patients were not allocated to ISAs at time ",
            lPltfTrial$lSnap$dCurrTime,
            " because no ISA is actively enrolling (anymore)."
          )
        )
        
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
