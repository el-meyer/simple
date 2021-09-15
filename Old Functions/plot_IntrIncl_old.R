plot.lIntrIncl <- function(x, dCurrTime = 1:52, accrual = "linear", accrual_param = 10,
                           intr_itt = "fixed", intr_itt_param = 10, intr_start = 1, ...) {
  
  # get matching function
  f <- match.fun(x$fnAddNewIntr)
  
  # Number of new patients at every time step
  if (accrual == "linear") {
    nNewPats <- rep(accrual_param, length(dCurrTime))
  }
  if (accrual == "exponential") {
    nNewPats <- round(accrual_param ^ dCurrTime)
  }
  if (accrual == "poisson") {
    nNewPats <- rpois(length(dCurrTime), accrual_param)
  }
  
  # Number of ISAs at start
  dActvIntr <- numeric(length(dCurrTime) + 1)
  dActvIntr[1] <- intr_start
  nInclIntr <- intr_start
  
  # Get Exit Times for ISAs at start
  dExitTimes <- NULL
  nExitIntr <- numeric(length(dCurrTime))
  
  for (i in 1:intr_start) {
    if (intr_itt == "fixed")  {dExitTimes[i] <- intr_itt_param}
    if (intr_itt == "random") {dExitTimes[i] <- rgeom(1, intr_itt_param) + 1}
  }
  
  # Initialize patients
  pats <- 0
  
  # Initialize last time a new ISA was entered
  dLastAddTime <- 1
  
  for (i in 1:length(dCurrTime)) {
    
    # Add new patients to patients after last inclusion
    nPatsPostAdd <- pats + nNewPats[i]
    
    # check how many ISAs leave the platform at this time point
    nExitIntr[i] <- sum(dExitTimes == i)
    
    # Check whether any more ISAs can be included
    if (x$nMaxIntr > nInclIntr) {
      
      # Replace outgoing ISAs
      nNewIntr_replace <- ifelse(x$bIntrRepl, nExitIntr[i], 0) 
      
      nNewIntr_extra <- 
        round(
          do.call(
            f, 
            args = list(
              lArgs        = x$lArgs, 
              dCurrTime    = dCurrTime[i], 
              dActvIntr    = dActvIntr[i],
              dLastAddTime = dLastAddTime,
              nPatsPostAdd = nPatsPostAdd
            )
          )
        )
      
      nNewIntr <- nNewIntr_replace + nNewIntr_extra
      
      # Make sure the number of ISAs included is not going to overshoot the maximum number of ISAs that can be added per timestep
      nNewIntr <- min(nNewIntr, x$nMaxAdd)
      
      # ADD DIFFERENCE TO A "QUEUE" ------------
      # NEED TO ADD CONCEPT OF MAXIMUM ARMS IN PARALLEL
      
      # Make sure number of ISAs to be included is not going to overshoot maximum number of ISAs that can be added during the platform trial
      nNewIntr <- min(nNewIntr, x$nMaxIntr - nInclIntr)
      
    } else {
      
      nNewIntr = 0
      
    }
    
    # Change number of active arms
    dActvIntr[i+1] <- dActvIntr[i] - nExitIntr[i] + nNewIntr
    
    if (nNewIntr > 0) {
      
      # adapt number of included ISAs so far
      nInclIntr <- nInclIntr + nNewIntr
      
      # switch last add time
      dLastAddTime <- i
      
      # add Exit Times
      for (j in (length(dExitTimes) + 1):(length(dExitTimes) + nNewIntr)) {
        if (intr_itt == "fixed")  {dExitTimes[j] <- i + intr_itt_param}
        if (intr_itt == "random") {dExitTimes[j] <- i + rgeom(1, intr_itt_param) + 1}
      }
    }
    
  }
  
  "%>%" <- dplyr::"%>%"
  
  mydata_pats <- 
    dplyr::tibble(
      Time       = dCurrTime,
      New        = nNewPats,
      Cumulative = cumsum(nNewPats)
    ) %>% 
    tidyr::pivot_longer(
      c("New", "Cumulative"),
      names_to = "PatientArrivals",
      values_to = "Number"
    )
  
  mydata_arms <- 
    dplyr::tibble(
      Time        = dCurrTime,
      Active      = dActvIntr[2:(length(dCurrTime) + 1)],
      Finished    = cumsum(nExitIntr)
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
    ggplot2::ggtitle("Simulated number of active arms over time") + 
    ggplot2::theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  g2 <- 
    ggplot2::ggplot(mydata_pats, ggplot2::aes(x = Time, y = Number, color = PatientArrivals)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Assumed Accrual over time") + 
    ggplot2::theme(
      legend.direction = "horizontal", 
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  g <- 
    ggpubr::ggarrange(
      g1, 
      g2,
      nrow = 2
    )
  
  print(g)
  
  invisible(list(mydata_arms, mydata_pats))
  
}
