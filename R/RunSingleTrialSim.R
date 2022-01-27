
#' @export

# Where to add data management to reduce runtime?

runSingleTrialSim <-
  function(
    lPltfDsgn # List that contains all the platform design rules
  ) {
    
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
    # Later, this should be OPTIONAL
    lSnapshots <- list()
    
    # Initially, platform trial is open 
    bTrialOpen <- TRUE
    
    # What happens every time unit of the platform trial?
    while (bTrialOpen) {
      
      # Handle ISA inclusion 
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
        "bTrialOpen",
        do.call(
          match.fun(lPltfDsgn$lFnDef$fnCheckTrialOpen),
          args = list(
            lPltfDsgn  = lPltfDsgn,
            lPltfTrial = lPltfTrial
          )
        )
      )
      
      # Update the time ("the week is over")
      # Think about more general "update snapshot" module?
      lPltfTrial$lSnap$dCurrTime <- lPltfTrial$lSnap$dCurrTime + 1
      
      # LATER: Put the following in some sort of clean up step
      # Store current snapshot outside of lPltfTrial
      lSnapshots <- c(lSnapshots, lPltfTrial$lSnap)
      
    }
    
    # Finally, create return object from platform trial object as specified
    # in fnWrapup
    
    assign(
      "ret",
      do.call(
        match.fun(lPltfDsgn$lFnDef$fnWrapup),
        args = list(
          lPltfDsgn  = lPltfDsgn,
          lPltfTrial = lPltfTrial,
          lSnapshots = lSnapshots
        )
      )
    )
    
    return(
      list(
        out = ret,
        snaps = lSnapshots
      )
    )
    
  }
