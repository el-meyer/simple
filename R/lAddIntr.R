#' Addition of ISAs into ongoing platform trial
#' 
#' Functions for creating, validating and simple use of class lAddIntr
#' 
#' @param fnAddIntr    Function which will add ISAs
#' @param lAddArgs     Further arguments used in fnAddIntr
#' 
#' @examples
#' 
#' @name lAddIntr
#' 
#' @export
#' @rdname lAddIntr
# Constructor Function
new_lAddIntr <- function(
  # Function that is used in adding ISAs to ongoing platform trial
  fnAddIntr = function(
    lPltfDsgn,  # List of platfom design parameters
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAddIntr function
  lAddArgs = list()
) {
  structure(
    list(
      fnAddIntr  = fnAddIntr,
      lAddArgs   = lAddArgs
    ),
    class        = "lAddIntr"
  )
}
#' @export
#' @rdname lAddIntr
# Validator Function
validate_lAddIntr <- function(x) {
  
  # Error if list is not of class lAddIntr
  if (class(x) != "lAddIntr") {
    stop(
      "Object is not of class lAddIntr."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnAddIntr", "lAddArgs"))) {
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
#' @rdname lAddIntr
# Helper Function
lAddIntr <- function(nTrt) {
  
  # Throw error if x is not a scalar
  if (!(is.atomic(nTrt) && length(nTrt) == 1L)) {
    stop("Supplied input is not a scalar")
  }
  
  # Throw error if x is not an integer
  if (!nTrt == round(nTrt)) {
    stop("Supplied input is not an integer")
  }
  
  new_lAddIntr(
    # In easy Version: Simply run lInitIntr on next ISA in ISA design list and append to platform trial list
    
    fnAddIntr = function(lPltfDsgn, lPltfTrial, lAddArgs) {
      
      # For every new ISA, run the respective lInitIntr function and add to lPltfTrial$isas
      for (i in 1:lAddArgs$nTrt) {
        
        lPltfTrial$isa <- 
          c(
            lPltfTrial$isa,
            list(
              do.call(
                match.fun(lPltfDsgn$lIntrDsgn[[length(lPltfTrial$isa) + 1]]$lInitIntr$fnInitIntr),
                args = 
                  list(
                    lPltfTrial = lPltfTrial,
                    lAddArgs   = lPltfDsgn$lIntrDsgn[[length(lPltfTrial$isa) + 1]]$lInitIntr$lAddArgs
                  )
              )
            )
          )
        
        print(
          paste0(
            "ISA ",
            length(lPltfTrial$isa),
            " was added to the trial at time ",
            lPltfTrial$lSnap$dCurrTime
          )
        )
        
      }
      
      # Return modified object
      return(lPltfTrial)
      
    },
    
    lAddArgs   = list(nTrt = nTrt)
  )
  
}

#' @export
#' @rdname lAddIntr
# Summary Function
summary.lAddIntr <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnAddIntr)))[2]
  
  cat("Specified accrual function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lAddArgs)
}
