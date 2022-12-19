#' ISA inclusion rules
#' 
#' Rules for ISA starting times of class lNewIntr
#' 
#' @param fnNewIntr      Function that checks how many new ISAs should be added to the platform
#' 
#' @param lAddArgs       Further arguments used in fnNewIntr
#' 
#' @examples
#' 
#' x <- lNewIntr(4)
#' validate_lNewIntr(x)
#' plot(x)
#' summary(x)
#' 
#' @name lNewIntr
#' 
#' @export
#' @rdname lNewIntr
# Constructor Function
new_lNewIntr <- 
  function(
    fnNewIntr = function(
      lPltfTrial,
      lAddArgs
    ) {},
    lAddArgs      = list()
  ) {
    structure(
      list(
        fnNewIntr = fnNewIntr,
        lAddArgs  = lAddArgs
      ),
      class       = "lNewIntr"
    )
  }
#' @export
#' @rdname lNewIntr
# Validator Function
validate_lNewIntr <- function(x) {
  
  # Error if list is not of class lNewIntr
  if (class(x) != "lNewIntr") {
    stop(
      "Object is not of class lNewIntr."
    )
  }
  
  # Check whether correct names
  if (!identical(names(x), c("fnNewIntr", "lAddArgs"))) {
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
#' @rdname lNewIntr
# Helper Function
lNewIntr <- function(nMaxIntr, nStartIntr, vArrTimes = NULL) {
  
  # Vector of arrival times can be unsorted
  
  if (!is.null(vArrTimes)) {
    # Throw error if vArrTimes is not a vector of integers
    if (!(is.atomic(vArrTimes) && typeof(vArrTimes) == "double")) {
      stop("Supplied vector is not scalar")
    }
    
    # Throw error if vArrTimes is not a vector of integers
    if (!all(vArrTimes == round(vArrTimes))) {
      stop("Supplied vector is not scalar")
    }
    
    # Throw error if not at least one entry is 1
    if (sum(vArrTimes == 1) == 0) {
      stop("At least one ISA needs to start in the beginning of the platform")
    }
  }
  
  # Throw error if x is not a scalar
  if (!(is.atomic(nMaxIntr) && length(nMaxIntr) == 1L)) {
    stop("Supplied input is not a scalar")
  }
  
  # Throw error if x is not an integer
  if (nMaxIntr != round(nMaxIntr)) {
     stop("Supplied input is not an interger")
  }
  
  # Throw error if x is not a scalar
  if (!(is.atomic(nStartIntr) && length(nStartIntr) == 1L)) {
    stop("Supplied input is not a scalar")
  }
  
  # Throw error if x is not an integer
  if (nStartIntr != round(nStartIntr)) {
    stop("Supplied input is not an interger")
  }
  
  # Throw error if maximum is larger than start value
  if (nStartIntr > nMaxIntr) {
    stop("Maximum number of ISAs is smaller than starting number of ISAs")
  }
  
  # In simple version: 
  # Replace outgoing ISAs only
  # Maximum x ISAs in total
  new_lNewIntr(
    fnNewIntr  = function(lPltfTrial, lAddArgs) {
      
      # either have vector of arrival times directly specified, or not
      if (is.null(lAddArgs$vArrTimes)) {
        # have special rules for first time unit
        if (lPltfTrial$lSnap$dCurrTime == 1) {
          dAdd <- lAddArgs$nStartIntr
          # if both an ISA was outgoing in this time step and the maximum number has not yet been reached
          # add as many ISAs as were outgoing
        } else if (lPltfTrial$lSnap$dExitIntr > 0 & length(lPltfTrial$lSnap$vIntrInclTimes) < lAddArgs$nMaxIntr) {
          # add as many as were outgoing but maximum as many as can still be added
          dAdd <- min(lAddArgs$nMaxIntr - length(lPltfTrial$lSnap$vIntrInclTimes), lPltfTrial$lSnap$dExitIntr)
        } else {
          dAdd <- 0
        }
        # if vector of arrival times directly specified
      } else {
        dAdd <- sum(lPltfTrial$lSnap$dCurrTime == lAddArgs$vArrTimes)
      }

      return(dAdd)
    },
    lAddArgs      = list(nMaxIntr = nMaxIntr, nStartIntr = nStartIntr, vArrTimes = vArrTimes)
  )
  
}

#' @export
#' @rdname lNewIntr
# Plot Function
# Expects vector of current times, assumptions regarding ISA in-trial-time
# ISA in-trial-time can be fixed (e.g. 10 time units) or random (probability at every time step)
# Number of ISAs at start can be chosen
# Assumes that there are always enough ISAs in the pipeline
plot.lNewIntr <- function(x, dCurrTime = 1:52, cIntrTime = "fixed", dIntrTimeParam = 10, nIntrStart = 1, ...) {
  
  if (dCurrTime[1] != 1) {
    stop("First element of dCurrTime is not 1.")
  }
  
  if (!cIntrTime      %in% c("fixed", "random") | 
      !nIntrStart     == round(nIntrStart)
      ) {
    stop("ISA specific parameters misspecified.")
  }
  
  if ((cIntrTime == "fixed"  & !dIntrTimeParam == round(dIntrTimeParam)) | 
      (cIntrTime == "random" & !dplyr::between(dIntrTimeParam, 0, 1))
  ) {
    stop("ISA in trial time type and parameter mismatch.")
  }
  
  # Get all relevant Input Arguments
  lInpArgs <- 
    list(
      dCurrTime = dCurrTime,
      ...
    )
  
  # All Input Arguments need to have the same length
  if (length(unique(sapply(lInpArgs, FUN = length))) != 1) {
      stop("Length of supplied global variables differs.")
    }
  
  # get matching function
  f <- match.fun(x$fnNewIntr)
  
  # Number of ISAs at start
  dActvIntr <- nIntrStart
  vIntrInclTimes <- rep(0, nIntrStart)
  
  # Initialize exit times
  dExitIntr <- 0
  vIntrExitTimes <- numeric(0)
  
  # Initialize counts
  vActvIntr <- numeric(length = length(dCurrTime) + 1)
  vActvIntr[1] <- nIntrStart
  vFinIntr <- numeric(length = length(dCurrTime) + 1)
  vFinIntr[1] <- 0
  
  for (i in 1:length(dCurrTime)) {
    
    # Check whether any ISAs are outgoing at this time point
    # Either fixed - then check inclusion times - or random - then draw randomly
    if (cIntrTime == "fixed") {
      dExitIntr <- sum(vIntrInclTimes == (i - dIntrTimeParam))
    }
    if (cIntrTime == "random") {
      dExitIntr <- rbinom(1, length(vIntrInclTimes) - length(vIntrExitTimes), dIntrTimeParam)
    }
    
    # Update vector of finished ISAs
    vFinIntr[i+1] <- vFinIntr[i] + dExitIntr
    
    # Add the ISA exit times to vector
    vIntrExitTimes <- c(vIntrExitTimes, rep(i, dExitIntr))
    
    lPltfTrial <- structure(
      list(
        lSnap = c(
          lapply(lInpArgs, FUN = function(x) x[[i]]), # extra global variables + current platform time
          list(
            dActvIntr      = dActvIntr, # number of ISAs active at beginning of current platform time
            dExitIntr      = dExitIntr, # number of outgoing ISAs at current platform time
            vIntrInclTimes = vIntrInclTimes, # vector of all ISA inclusion times so far
            vIntrExitTimes = vIntrExitTimes # vector of all ISA exit times so far
          )
        )
      ), 
      class = "lPltfTrial"
    )
    
    # call function to determine how many new ISAs to include
    dIntrAdd <- 
      round(
        do.call(
          f, 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = x$lAddArgs
          )
        )
      )
    
    # Add ISA Inclusion Times
    vIntrInclTimes <- c(vIntrInclTimes, rep(i, dIntrAdd))
    
    # Change number of active arms
    dActvIntr <- length(vIntrInclTimes) - length(vIntrExitTimes)
    
    # Update vector of active ISAs
    vActvIntr[i+1] <- dActvIntr
    
  }
  
  "%>%" <- dplyr::"%>%"
  
  mydata_arms <- 
    dplyr::tibble(
      Time        = c(0, dCurrTime),
      Active      = vActvIntr,
      Finished    = vFinIntr
    ) %>% 
    tidyr::pivot_longer(
      c("Active", "Finished"),
      names_to = "ISAs",
      values_to = "Number"
    )
  
  g1 <- 
    ggplot2::ggplot(mydata_arms, ggplot2::aes(x = Time, y = Number, color = ISAs)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Simulated number of active ISAs over time") + 
    ggplot2::theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  print(g1)
  
  invisible(mydata_arms)
  
}

#' @export
#' @rdname lNewIntr
# Summary Function
summary.lNewIntr <- function(x, ...) {
  
  body <- as.character(body(match.fun(x$fnNewIntr)))[2]
  
  cat("Specified inclusion function: \n")
  print(body)
  cat("\n Specified arguments: \n")
  print(x$lAddArgs)
  
}
