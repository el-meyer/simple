#' Recruitment Parameters
#' 
#' Functions for creating, validating and simple use of class lRecrPars
#' 
#' @param fnRecrPars   Function which will simulate number of new patient arrivals in time step
#'                     dependent on the the current global variables
#' @param lAddArgs     Further arguments used in fnRecrPars
#' 
#' @examples
#' 
#' x <- lRecrPars(4)
#' validate_lRecrPars(x)
#' plot(x)
#' summary(x)
#' 
#' @name lRecrPars
#' 
#' @export
#' @rdname lRecrPars
# Constructor Function
new_lRecrPars <- function(
  # Function that is used in recruitment
  fnRecrProc = function(
    lPltfTrial, # List of current trial progress
    lAddArgs    # List of further arguments for this module
    ) {}, 
  # List of Arguments used with fnRecrProc function
  lAddArgs = list()
  ) {
  structure(
    list(
      fnRecrProc = fnRecrProc,
      lAddArgs   = lAddArgs
     ),
    class        = "lRecrPars"
  )
}
#' @export
#' @rdname lRecrPars
# Validator Function
validate_lRecrPars <- function(x) {
  
  # Error if list is not of class lRecrPars
  if (class(x) != "lRecrPars") {
    stop(
      "Object is not of class lRecrPars."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnRecrProc", "lAddArgs"))) {
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
#' @rdname lRecrPars
# Helper Function
lRecrPars <- function(nPat) {
  
  # Throw error if x is not a scalar
  if (!(is.atomic(nPat) && length(nPat) == 1L)) {
    stop("Supplied input is not a scalar")
  }
  
  # Throw error if x is not an integer
  if (!nPat == round(nPat)) {
    stop("Supplied input is not an integer")
  }
  
  new_lRecrPars(
    # In easy Version: Simply number or participants
    fnRecrProc = function(lPltfTrial, lAddArgs) {lAddArgs$nPat},
    lAddArgs   = list(nPat = nPat)
  )

}

#' @export
#' @rdname lRecrPars
# Plot Function
# Expects vector of current times and all other snapshot variables used in the function definition
# Default is Time 1-52 
plot.lRecrPars <- function(x, dCurrTime = 1:52, ...) {
  
  # Get all Input Arguments except for x
  lInpArgs <- 
    list(
      dCurrTime = dCurrTime,
      ...
    )
  
  # All Input Arguments need to have the same length
  if (length(unique(sapply(lInpArgs, FUN = length))) != 1) {
    stop("Length of supplied snapshot variables differs.")
  }

  y <- numeric(length(dCurrTime))
  f <- match.fun(x$fnRecrProc)
  
  for (i in 1:length(dCurrTime)) {
    
    # Get the i-th elements of the global variable list vectors and define these as current global variables
    lPltfTrial <- structure(list(lSnap = lapply(lInpArgs, FUN = function(x) x[[i]])), class = "lPltfTrial")
    
    y[i] <- 
      round(
        do.call(
          f, 
          args = list(
            lAddArgs  = x$lAddArgs, 
            lPltfTrial = lPltfTrial
          )
        )
      )
  }
  
  "%>%" <- dplyr::"%>%"
  
  mydata <- 
    dplyr::tibble(
      Time       = dCurrTime,
      New        = y
    ) %>% 
    dplyr::mutate(
      Cumulative = cumsum(y)
    ) %>% 
    tidyr::pivot_longer(
      c("New", "Cumulative"),
      names_to = "AccruedPatients",
      values_to = "Number"
    )
  
  g1 <- 
    ggplot2::ggplot(mydata, ggplot2::aes(x = Time, y = Number, color = AccruedPatients)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Simulated accrued Patients over time with respect to specified platform trajectory")
  
  print(g1)
  
  invisible(mydata)
  
}

#' @export
#' @rdname lRecrPars
# Summary Function
summary.lRecrPars <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnRecrProc)))[2]
  
  cat("Specified accrual function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lAddArgs)
}

