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
#' 
#' @name lRecrPars
#' 
#' @export
#' @rdname lRecrPars
# Constructor Function
new_lRecrPars <- function(
  # Function that is used in recruitment
  fnRecrProc = function(
    lGlobVars,  # List of global Variables
    lAddArgs    # List of further arguments for function
  ) {}, 
  # List of Arguments used with fRecrProc function
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
  
  # Error if length of list < 2
  if (length(x) < 2) {
    stop(
      "Too few attributes were specified."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnRecrProc", "lAddArgs"))) {
    stop(
      "Wrong names."
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
  if (!"lGlobVars" %in% names(f_args) | !"lAddArgs" %in% names(f_args)) {
    stop(
      "Function not properly specified."
    )
  }
  
  # Warnings if length of list > 2
  if (length(x) > 2) {
    warning(
      "Too many attributes were specified; ignoring additional attributes."
    )
  }
  
}
#' @export
#' @rdname lRecrPars
# Helper Function
lIntrIncl <- function(dRandAddProb) {
  
  # Error if supplied probability is not between 0 and 1
  if (dRandAddProb < 0 | dRandAddProb > 1) {
    stop("Probability needs to be between 0 and 1.")
  }
  
  # In simple version: 
  # Replace outgoing ISAs only
  # Maximum 10 ISAs in total
  new_lIntrIncl(
    bIntrRepl     = TRUE, # Replace exiting ISAs
    nMaxIntr      = 10,   # Add maximum 10 ISAs during course of trial
    nMaxAdd       = 2,    # Add maximum two ISAs at the same time
    fnAddNewIntr  = function(dCurrTime, dLastAddTime, dActvIntr, nPatsPostAdd, lArgs) {
      return(sample(0:1, size = 1, prob = c(1 - lArgs$dRandAddProb, lArgs$dRandAddProb)))
    },                    # Add ISAs with a certain probability in every time unit
    lAddArgs      = list(dRandAddProb = dRandAddProb)
  )
  
}

#' @export
#' @rdname lRecrPars
# Plot Function
# Expects vector of current times and active arms
# Default is Time 1-52 and always one active arm
plot.lRecrPars <- function(x, dCurrTime = 1:52, dActvIntr = rep(1, 52), ...) {
  
  # Get all Input Arguments except for x
  lInpArgs <- 
    list(
      dCurrTime = dCurrTime,
      dActvIntr = dActvIntr,
      ...
    )
  
  # All Input Arguments need to have the same length
  if (length(unique(sapply(lInpArgs, FUN = length))) != 1) {
    stop("Length of supplied global variables differs.")
  }
  
  y <- numeric(length(dCurrTime))
  f <- match.fun(x$fnRecrProc)
  
  for (i in 1:length(dCurrTime)) {
    
    # Get the i-th elements of the global variable list vectors and define these as current global variables
    lGlobVars <- structure(list(lVars = lapply(lInpArgs, FUN = function(x) x[[i]])), class = "lGlobVars")
    
    y[i] <- 
      round(
        do.call(
          f, 
          args = list(
            lAddArgs  = x$lAddArgs, 
            lGlobVars = lGlobVars
          )
        )
      )
  }
  
  "%>%" <- dplyr::"%>%"
  
  mydata <- 
    dplyr::tibble(
      Time       = dCurrTime,
      ActiveArms = dActvIntr,
      New        = y
    ) %>% 
    dplyr::mutate(
      Cumulative = cumsum(y)
    ) %>% 
    tidyr::pivot_longer(
      c("New", "Cumulative"),
      names_to = "PatientArrivals",
      values_to = "Number"
    )
  
  g1 <- 
    ggplot2::ggplot(mydata, ggplot2::aes(x = Time, y = Number, color = PatientArrivals)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Accrued Patients over time")
  
  g2 <- 
    ggplot2::ggplot(mydata, ggplot2::aes(x = Time, y = ActiveArms)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Assumed number of active arms over time")
  
  
  g <- 
    ggpubr::ggarrange(
      g1, 
      g2,
      nrow = 2,
      common.legend = TRUE
    )
  
  print(g)
  
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

