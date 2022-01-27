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
  fnCheckTrialOpen  = function(
    lPltfDsgn,
    lPltfTrial
  ) {}
) {
  structure(
    list(
      fnInitialize     = fnInitialize,
      fnAddNewIntr     = fnAddNewIntr,
      fnGenNewPatData  = fnGenNewPatData,
      fnCheckTrialOpen = fnCheckTrialOpen
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
        
        # Initialize dataset of isa using the appropriate function
        assign(
          "isa",
          do.call(
            match.fun(lPltfDsgn$lInitIntr$fnInitTrt),
            args = list(
              lPltfDsgn  = lPltfDsgn,
              lAddArgs   = lPltfDsgn$lInitIntr$lAddArgs
            )
          )
        )
        
        # initiate patient data set
        assign(
          "patients",
          do.call(
            match.fun(lPltfDsgn$lInitPat$fnInitPat),
            args = list(
              lPltfDsgn  = lPltfDsgn,
              lAddArgs   = lPltfDsgn$lInitPat$lAddArgs
            )
          )
        )
        
        # create initial "snapshot"
        snap <- 
          list(
            dCurrTime       = 0, # current time
            dActvIntr       = length(isa), # current number of active cohorts (== enrolling)
            dExitIntr       = 0, # number of outgoing ISAs at this time point
            vIntrInclTimes  = rep(0, length(isa)), # vector of all ISA inclusion times so far
            vIntrExitTimes  = rep(NA, length(isa)), # vector of all ISA exit times so far
            vCurrAllocRatio = isa$overview$alloc, # Vector of current allocation ratio between ISAs
            nAnalysisOver   = 0 # Overall number of analyses already conducted
          )
        
        lPltfTrial <- 
          list(
            isa       = isa,
            patients  = patients,
            lSnap     = snap
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
              match.fun(lPltfDsgn$lIntrIncl$fnIntrIncl), 
              args = list(
                lPltfTrial   = lPltfTrial,
                lAddArgs     = lPltfDsgn$lIntrIncl$lAddArgs
              )
            ),
            nIntrLeft
          )
        
        # For every new ISA, perform the inclusion step
        # For now: Just add treatment in treatment data frame and make a new list
        if (nNewIntr > 0) {
          
          isas_new <- list()
          for (i in 1:nNewIntr) {
            
            isas_new[[i]] <- 
              list(
                id           = length(isa) + i, # assign IDs accoring to how many ISAs start
                name         = lPltfDsgn$lIntrDsgn[[length(isa) + i]]$name, # get the first names
                n_cur        = 0, # Current sample size allocated to this ISA
                enrol        = TRUE, # is ISA currently enrolling
                alloc        = 1, # allocation ratio relative to other ISAs
                start_time   = lPltfTrial$lSnap$dCurrTime, # at what calendar time was ISA started
                nAnalysis    = 0, # number of analyses conducted for this ISA
                end_time     = NA, # at what calendar time was ISA stopped
                end_reason   = NA # reason why ISA was stopped
              )
            
          }
          
          isa <- 
            c(
              isa,
              isas_new
            )

        }
        
      }
      
      # Return modified list object
      return(lPltfTrial)
      
    },
    
    
    # Update between ISA and within ISA Allocation Ratio
    fnUpdateAlloc = function(
      lPltfDsgn, 
      lPltfTrial
    ) {
      
      # Update between ISA allocation ratio: Apply function from lIntrAlloc
      lPltfTrial <- 
        do.call(
          match.fun(lPltfDsgn$lIntrAlloc$fnIntrAlloc), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lIntrAlloc$lAddArgs
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

      newdat_list <- list()
      
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
      
      newdat_df <- do.call(rbind.data.frame, newdat)
      
      
      # Assign patients to ISAs and within ISAs
      
      # Simulate the patients' outcomes
      # Those dataframes are stored separately
      
      # Append to data frame
      
    },
    
    # Steps that should be performed for each ISA
    fnIntrAction = function(
      lPltfDsgn,
      lPltfTrial
    ) {
      
      # Check Analysis Milestone
      
      # Run Analyses if necessary
      
      # Implement possible decisions
      
      # Check whether ISA is still actively enrolling
      
    },
    
    # Check if platform should be closed
    fnCheckTrialOpen = function(
      lPltfDsgn,
      lPltfTrial
    ) {
      
      # at the moment, just look at module that checks if stopping rules are reached and 
      # return logical
      
      bTrialOpen <- 
        do.call(
          match.fun(lPltfDsgn$lStopRule$fnStopRule), 
          args = list(
            lPltfTrial   = lPltfTrial,
            lAddArgs     = lPltfDsgn$lStopRule$lAddArgs
          )
        )
      
      return(bTrialOpen)  
    }
    
    
  )
  
}
