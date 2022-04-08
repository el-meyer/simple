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
  
  # Error if list is not of class lAllocIntr
  if (class(x) != "lAllocIntr") {
    stop(
      "Object is not of class lAllocIntr."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnAllocIntr", "lAddArgs"))) {
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
  if (!"lPltfDsgn" %in% names(f_args) | !"lPltfTrial" %in% names(f_args) | !"lAddArgs" %in% names(f_args)) {
    stop(
      "Function not properly specified."
    )
  }
  
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

#' @export
#' @rdname lAllocIntr
# Summary Function
summary.lAddPats <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnAllocIntr)))[2]
  
  cat("Specified accrual function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lAllocIntr)
}
