
#' @export

# Where to add data management to reduce runtime?

fnRunSingleTrialSim <-
  function(
    lPltfDsgn,                # List that contains all the platform design rules
    bRetainSnaps = TRUE,      # Whether or not to keep the list of snapshots
    bCreateLog   = TRUE,      # Whether or not to create a log file in current folder
    cLogName     = "platform" # Name of Log File
  ) {
    
    # Check whether or not to create a log
    if (bCreateLog) {
      # Delte possibly already existing log file
      if (file.exists(paste0(cLogName, ".log"))) {
        file.remove(paste0(cLogName, ".log"))
      }
      sink(paste0(cLogName, ".log"), append = TRUE, split = FALSE)
    } else {
      sink("nul", split = FALSE)
    }
    
    # Start by initializing the platform trial element, lPltfTrial, 
    # according to the algorithm specified in fnInitialize
    
    assign(
      "lPltfTrial",
      do.call(
        match.fun(lPltfDsgn$lFnDef$fnInitialize),
        args = list(
          lPltfDsgn
        )
      )
    )
    
    # Create list of snapshots to be stored outside of lPltfTrial
    if (bRetainSnaps) {
      lSnapshots <- list()
    }
    
    # Initially, platform trial is open 
    bTrialClose <- FALSE
    
    # What happens every time unit of the platform trial?
    while (!bTrialClose) {
      
      # Update snapshot
      assign(
        "lPltfTrial",
        do.call(
          match.fun(lPltfDsgn$lSnap$fnSnap),
          args = list(
            lPltfTrial = lPltfTrial,
            lAddArgs   = lPltfDsgn$lSnap$lAddArgs
          )
        )
      )
      
      # Handle update of allocation ratios
      assign(
        "lPltfTrial",
        do.call(
          match.fun(lPltfDsgn$lFnDef$fnUpdateAlloc),
          args = list(
            lPltfDsgn  = lPltfDsgn,
            lPltfTrial = lPltfTrial
          )
        )
      )
      
      # Handle patient inclusion
      assign(
        "lPltfTrial",
        do.call(
          match.fun(lPltfDsgn$lFnDef$fnGenNewPat),
          args = list(
            lPltfDsgn  = lPltfDsgn,
            lPltfTrial = lPltfTrial
          )
        )
      )
      
      # Handle ISA actions
      assign(
        "lPltfTrial",
        do.call(
          match.fun(lPltfDsgn$lFnDef$fnIntrAction),
          args = list(
            lPltfDsgn  = lPltfDsgn,
            lPltfTrial = lPltfTrial
          )
        )
      )
      
      # Check whether platform trial is still open, using
      # the rules specified in fnCheckTrialOpen
      assign(
        "bTrialClose",
        do.call(
          match.fun(lPltfDsgn$lFnDef$fnCheckTrialClose),
          args = list(
            lPltfDsgn  = lPltfDsgn,
            lPltfTrial = lPltfTrial
          )
        )
      )
      
      if (bRetainSnaps) {
        # Store current snapshot outside of lPltfTrial
        lSnapshots <- c(lSnapshots, list(lPltfTrial$lSnap))
      }
      
      if (!bTrialClose) {
        # Handle ISA inclusion if platform did not stop
        assign(
          "lPltfTrial",
          do.call(
            match.fun(lPltfDsgn$lFnDef$fnAddNewIntr),
            args = list(
              lPltfDsgn  = lPltfDsgn,
              lPltfTrial = lPltfTrial
            )
          )
        )
        
        # Update the time ("the week is over")
        # Think about more general "update snapshot" module?
        lPltfTrial$lSnap$dCurrTime <- lPltfTrial$lSnap$dCurrTime + 1
        
      }
      
    }
    
    # Finally, create return object from platform trial object as specified
    # in fnWrapup
    
    # Create final snapshot 
    assign(
      "lFinalSnap",
      do.call(
        match.fun(lPltfDsgn$lSnap$fnSnap),
        args = list(
          lPltfTrial = lPltfTrial,
          lAddArgs   = lPltfDsgn$lSnap$lAddArgs
        )
      )
    )
    
    assign(
      "out",
      do.call(
        match.fun(lPltfDsgn$lFnDef$fnWrapup),
        args = list(
          lPltfDsgn  = lPltfDsgn,
          lPltfTrial = lPltfTrial
        )
      )
    )
    
    # Create Summary as initial return object
    ret <-
      list(
        lFinalSnap   = lFinalSnap
      )
    
    # Include List of Snapshots or extra output if needed
    if (bRetainSnaps) {
      ret$lSnapshots <- lSnapshots
    }
    if (!is.null(out)) {
      ret$lSummary <- out
    }
    
    sink()
    
    # Return return list
    return(
      ret
    )
    
  }
