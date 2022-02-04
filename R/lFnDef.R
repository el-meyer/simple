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
      
        # Add new ISAs according to how many should start
        assign(
          "lPltfTrial",
          do.call(
            match.fun(lPltfDsgn$lAddIntr$fnAddIntr),
            args = list(
              lPltfDsgn  = lPltfDsgn,
              lPltfTrial = lPltfTrial,
              lAddArgs   = lPltfDsgn$lAddIntr$lAddArgs
            )
          )
        )
        
        return(
          lPltfTrial
        )
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
          match.fun(lPltfDsgn$lIntrAlloc$fnIntrAlloc), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lIntrAlloc$lAddArgs
          )
        )
      
      # Append to data frames in ISAs
      # Necessary because structure of assignment to arms and
      # outcome simulation can differ in ISAs and result in 
      # different numbers of columns
      lPltfTrial <-
        do.call(
          match.fun(lPltfDsgn$lAddPats$fnAddPats), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lAddPats$lAddArgs
          )
        )
        
      # Assign patients within ISAs
        
      # Simulate the patients' outcomes
      
      return(lPltfTrial)
      
    },
    
    # # Steps that should be performed for each ISA
    fnIntrAction = function(
      lPltfDsgn,
      lPltfTrial
    ) {
    #   
    #   # Check Analysis Milestone
    #   
    #   # Run Analyses if necessary
    #   
    #   # Implement possible decisions
    #   
    #   # Check whether ISA is still actively enrolling
    
      # For now, do nothing
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
      
      # at the moment, just return lPltfTrial
      lPltfTrial$isa
    }
    
    
  )
  
}
