#' Check ISA Enrollment
#' 
#' Functions and rules for within ISA checking of Enrollment of class lCheckEnrl
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lCheckEnrl
#' 
#' @export
#' @rdname lCheckEnrl
# Constructor Function
new_lCheckEnrl <- function(
  # Function that is used in checking ISA Enrollment
  fnCheckEnrl = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnCheckEnrl function
  lAddArgs = list()
) {
  structure(
    list(
      fnCheckEnrl   = fnCheckEnrl,
      lAddArgs      = lAddArgs
    ),
    class           = "lCheckEnrl"
  )
}
#' @export
#' @rdname lCheckEnrl
# Validator Function
validate_lCheckEnrl <- function(x) {
  
  # Error if list is not of class lCheckEnrl
  if (class(x) != "lCheckEnrl") {
    stop(
      "Object is not of class lCheckEnrl."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnCheckEnrl", "lAddArgs"))) {
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
#' @rdname lCheckEnrl
# Helper Function
lCheckEnrl <- function() {
  
  new_lCheckEnrl(
    fnCheckEnrl = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      
      # By default just check if final sample size was reached OR the ISA already has a stopping reason or nEndTime
      
      if (
        lPltfTrial$isa[[lAddArgs$current_id]]$nMaxNIntr <= 
        nrow(do.call(rbind.data.frame, lPltfTrial$isa[[lAddArgs$current_id]]$lPats))
        ) {
        
        if (lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl) {
          # If this is the first time unit in which the enrollment is not active
          print(
            paste0(
              "ISA ",
              lAddArgs$current_id,
              " has stopped enrollment at time ",
              lPltfTrial$lSnap$dCurrTime
            )
          )
          
          lPltfTrial$lSnap$dExitIntr <- lPltfTrial$lSnap$dExitIntr + 1
          lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl <- FALSE
          lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime <- lPltfTrial$lSnap$dCurrTime
          
        }
        
        # If stopping either because an end reason already exists or the time has been reached
        
      } else {
        
        if (!is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason) |
            !is.na(lPltfTrial$isa[[lAddArgs$current_id]]$nEndTime)
            ) {
          
          if (lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl) {
            # If this is the first time unit in which the enrollment is not active
            print(
              paste0(
                "ISA ",
                lAddArgs$current_id,
                " has stopped enrollment at time ",
                lPltfTrial$lSnap$dCurrTime
              )
            )
            
            lPltfTrial$lSnap$dExitIntr <- lPltfTrial$lSnap$dExitIntr + 1
            lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl <- FALSE
            lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime <- lPltfTrial$lSnap$dCurrTime
            
            if (is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
              lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Other"
            }
            
          }
          
        }
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}

#' @export
#' @rdname lCheckEnrl
# Summary Function
summary.lCheckEnrl <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnCheckEnrl)))[2]
  
  cat("Specified accrual function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lCheckEnrl)
}
