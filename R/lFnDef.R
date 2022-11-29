#' List of Function Definitions
#' 
#' Different ISA Designs to draw from of class lIntrDsgnList
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lFnDef
#' 
#' @export
#' @rdname lFnDef
# Constructor Function
new_lFnDef <- function(
  fnInitialize      = function(
    lPltfDsgn
  ) {},
  fnAddNewIntr      = function(
    lPltfDsgn, 
    lPltfTrial
  ) {},
  fnUpdateAlloc     = function(
    lPltfDsgn, 
    lPltfTrial
  ) {},
  fnGenNewPatData   = function(
    lPltfDsgn,
    lPltfTrial
  ) {},
  fnIntrAction      = function(
    lPltfDsgn,
    lPltfTrial
  ) {},
  fnCheckTrialClose = function(
    lPltfDsgn,
    lPltfTrial
  ) {},
  fnWrapup = function(
    lPltfDsgn,
    lPltfTrial
  ) {}
) {
  structure(
    list(
      fnInitialize      = fnInitialize,
      fnAddNewIntr      = fnAddNewIntr,
      fnUpdateAlloc     = fnUpdateAlloc,
      fnGenNewPatData   = fnGenNewPatData,
      fnIntrAction      = fnIntrAction,
      fnCheckTrialClose = fnCheckTrialClose,
      fnWrapup          = fnWrapup
    ),
    class       = "lFnDef"
  )
}
#' @export
#' @rdname lFnDef
# Validator Function
validate_lFnDef <- function(x) {
  
}
#' @export
#' @rdname lFnDef
# Helper Function
lFnDef <- function() {
  
  new_lFnDef(
    
    # Initialization Function
    fnInitialize = function(
        lPltfDsgn
      ) {
        
      # Initialize empty list objects and set time to 0
      lPltfTrial <- list()
      lPltfTrial$lSnap$dCurrTime <- 0
      
      return(lPltfTrial)
      
      },
    
    # Add new ISAs
    fnAddNewIntr = function(
      lPltfDsgn, 
      lPltfTrial
    ) {

      # For now simplicity assumption: If lPtlfTrial$lIntrDsgn list is not empty, those are candidates to be
      # included; no modeling of waiting list etc. Also this list specifies exactly the order in which ISAs
      # are included
      
      # number of isa that could still be added
      nIntrLeft <- length(lPltfDsgn$lIntrDsgn) - length(lPltfTrial$isa)
      
      # Check whether any new treatment can be added
      if (nIntrLeft > 0) {
        
        # Check how many new ISAs should be entered into platform at this time step
        # also checking whether this number is still available
        nNewIntr <- 
          min(
            do.call(
              match.fun(lPltfDsgn$lNewIntr$fnNewIntr), 
              args = list(
                lPltfTrial   = lPltfTrial,
                lAddArgs     = lPltfDsgn$lNewIntr$lAddArgs
              )
            ),
            nIntrLeft
          )
        
        # For every new ISA, perform the inclusion step
        # For now: Just add treatment in treatment data frame and make a new list
        if (nNewIntr > 0) {
          
          lPltfTrial <-
            do.call(
              match.fun(lPltfDsgn$lAddIntr$fnAddIntr),
              args = list(
                lPltfDsgn  = lPltfDsgn,
                lPltfTrial = lPltfTrial,
                lAddArgs   = list(nTrt = nNewIntr)
              )
            )
        }
      }
      
      # Return modified list object
      return(lPltfTrial)
      
    },
    
    
    # Update between ISA Allocation Ratio
    fnUpdateAlloc = function(
      lPltfDsgn, 
      lPltfTrial
    ) {
      
      # Create empty temp list in Snap
      lPltfTrial$lSnap$isa_temp <- vector("list", length(lPltfTrial$isa))
      
      # Update between ISA allocation ratio: Apply function from lIntrAlloc
      lPltfTrial <- 
        do.call(
          match.fun(lPltfDsgn$lUpdIntrAlloc$fnUpdIntrAlloc), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lUpdIntrAlloc$lAddArgs
          )
        )

      # Return modified list object
      return(lPltfTrial)
      
    },
    
    
    # Add new Patients
    fnGenNewPat = function(
      lPltfDsgn, 
      lPltfTrial
    ) {
      
      # Check how many new patients arrive this time step
      nNewPats <- 
        do.call(
          match.fun(lPltfDsgn$lRecrPars$fnRecrProc), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lRecrPars$lAddArgs
          )
        )
      
      
      # Simulate the patients' baseline variables
      # Programmed initially via list so we do not need to know how 
      # many columns and their names
      
      if (nNewPats > 0) {
        
        newdat <- list()
        
        for (i in 1:nNewPats) {
          newdat[[i]] <- 
            do.call(
              match.fun(lPltfDsgn$lSimBase$fnSimBase), 
              args = list(
                lPltfTrial   = lPltfTrial,
                lAddArgs     = lPltfDsgn$lSimBase$lAddArgs
              )
            )
        }
        
        # Add new patients to lSnap
        lPltfTrial$lSnap$newdat_df <- do.call(rbind.data.frame, newdat)
        
        # Assign patients to ISAs
        lPltfTrial <-
          do.call(
            match.fun(lPltfDsgn$lAllocIntr$fnAllocIntr), 
            args = list(
              lPltfTrial   = lPltfTrial,
              lAddArgs     = lPltfDsgn$lAllocIntr$lAddArgs
            )
          )
        
        # Move to ISAs
        # Necessary because structure of assignment to arms and
        # outcome simulation can differ in ISAs and result in 
        # different numbers of columns
        # Only those ISAs that actually received Pats this timestep
        for (i in unique(lPltfTrial$lSnap$newdat_df$ISA)) {
          lPltfTrial$isa[[i]]$tempPats <-
            subset(lPltfTrial$lSnap$newdat_df, ISA == i)
        }
        
        # Assign patients within ISAs
        for (i in unique(lPltfTrial$lSnap$newdat_df$ISA)) {
          lPltfTrial <-
            do.call(
              match.fun(lPltfDsgn$lIntrDsgn[[i]]$lAllocArm$fnAllocArm), 
              args = list(
                lPltfTrial   = lPltfTrial,
                lAddArgs     = c(
                  lPltfDsgn$lIntrDsgn[[i]]$lAllocArm$lAddArgs,
                  current_id = i 
                )
              )
            )
        }
        
        # Simulate the patients' outcomes
        for (i in unique(lPltfTrial$lSnap$newdat_df$ISA)) {
          lPltfTrial <-
            do.call(
              match.fun(lPltfDsgn$lIntrDsgn[[i]]$lPatOutcome$fnPatOutcome), 
              args = list(
                lPltfTrial   = lPltfTrial,
                lAddArgs     = c(
                  lPltfDsgn$lIntrDsgn[[i]]$lPatOutcome$lAddArgs,
                  current_id = i 
                )
              )
            )
        }
        
        # Append to real dataset and remove the temp data
        for (i in unique(lPltfTrial$lSnap$newdat_df$ISA)) {
          lPltfTrial$isa[[i]]$lPats <-
            c(
              lPltfTrial$isa[[i]]$lPats,
              list(lPltfTrial$isa[[i]]$tempPats)
            )
          lPltfTrial$isa[[i]]$tempPats <- NULL
        }
        
      }
      
      return(lPltfTrial)
      
    },
    
    # # Steps that should be performed for each ISA
    fnIntrAction = function(
      lPltfDsgn,
      lPltfTrial
    ) {
      
      for (i in 1:length(lPltfTrial$isa)) {

        # Check Analysis Milestone
        lPltfTrial <-
          do.call(
            match.fun(lPltfDsgn$lIntrDsgn[[i]]$lCheckAnlsMstn$fnCheckAnlsMstn), 
            args = list(
              lPltfTrial   = lPltfTrial,
              lAddArgs     = c(
                lPltfDsgn$lIntrDsgn[[i]]$lCheckAnlsMstn$lAddArgs,
                current_id = i 
              )
            )
          )
        
        # If any analysis milestone reached
        if (any(lPltfTrial$lSnap$isa_temp[[i]]$AnlsMstn)) {
          # check how many analyses were conducted already and if this number is smaller than 
          # the total number of milestones reached
          if (sum(lPltfTrial$lSnap$isa_temp[[i]]$AnlsMstn) > 
              length(lPltfTrial$isa[[i]]$lAnalyses)) {
            
            # Run Analysis
            lPltfTrial <-
              do.call(
                match.fun(lPltfDsgn$lIntrDsgn[[i]]$lAnls$fnAnls),
                args = list(
                  lPltfTrial   = lPltfTrial,
                  lAddArgs     = c(
                    lPltfDsgn$lIntrDsgn[[i]]$lAnls$lAddArgs,
                    current_id = i,
                    # Pass latest Milestone to Analysis Function
                    nMstn      = sum(lPltfTrial$lSnap$isa_temp[[i]]$AnlsMstn)
                  )
                )
              )
           
          }
        }
        
        # Implement possible Decisions
        lPltfTrial <-
          do.call(
            match.fun(lPltfDsgn$lIntrDsgn[[i]]$lSynthRes$fnSynthRes),
            args = list(
              lPltfTrial   = lPltfTrial,
              lAddArgs     = c(
                lPltfDsgn$lIntrDsgn[[i]]$lSynthRes$lAddArgs,
                current_id = i
              )
            )
          )

        # Check whether ISA is still actively enrolling
        lPltfTrial <-
          do.call(
            match.fun(lPltfDsgn$lIntrDsgn[[i]]$lCheckEnrl$fnCheckEnrl), 
            args = list(
              lPltfTrial   = lPltfTrial,
              lAddArgs     = c(
                lPltfDsgn$lIntrDsgn[[i]]$lCheckEnrl$lAddArgs,
                current_id = i 
              )
            )
          )
        
      }
    
      return(lPltfTrial)
      
      },
    
    # Check if platform should be closed
    fnCheckTrialClose = function(
      lPltfDsgn,
      lPltfTrial
    ) {
      
      # at the moment, just look at module that checks if stopping rules are reached and 
      # return logical
      
      bTrialClose <- 
        do.call(
          match.fun(lPltfDsgn$lStopRule$fnStopRule), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lStopRule$lAddArgs
          )
        )
      
      return(bTrialClose)  
    },
    
    # Create return Object
    fnWrapup = function(
      lPltfDsgn,
      lPltfTrial
    ) {
      
      print(
        paste0(
          "The platform was stopped at time ",
          lPltfTrial$lSnap$dCurrTime
        )
      )
      
      # Run Wrapup
      ret <-
        do.call(
          match.fun(lPltfDsgn$lPltfSummary$fnPltfSummary), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lPltfSummary$lAddArgs
          )
        )
      
      return(ret)
      
    }
    
    
  )
  
}
