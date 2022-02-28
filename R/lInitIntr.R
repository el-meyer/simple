#' Initialization of ISA
#' 
#' Functions for creating, validating and simple use of class lInitIntr
#' 
#' @param fnInitIntr    Function which will initiate the ISA
#' @param lAddArgs  Further arguments used in fnInitIntr
#' 
#' @examples
#' 
#' @name lInitIntr
#' 
#' @export
#' @rdname lInitIntr
# Constructor Function
new_lInitIntr <- function(
  # Function that is used in initializing of ISA
  fnInitIntr = function(
    lPltfTrial,  # List of current platform trial status
    lAddArgs     # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnInitIntr function
  lAddArgs = list()
) {
  structure(
    list(
      fnInitIntr     = fnInitIntr,
      lAddArgs       = lAddArgs
    ),
    class            = "lInitIntr"
  )
}
#' @export
#' @rdname lInitIntr
# Validator Function
validate_lInitIntr <- function(x) {
  
}
#' @export
#' @rdname lInitIntr
# Helper Function
# By default balanced allocation within ISA 
lInitIntr <- function(
  cIntrName = "I_NoName", 
  cArmNames = c("ArmNoName"),
  nMaxNIntr = 100,
  vRandList = 
    as.vector(
      replicate(
        n    = ceiling(nMaxNIntr / length(cArmNames)),
        expr = sample(cArmNames)
      )
    ),
  vMaxNArms = rep(ceiling(nMaxNIntr / length(cArmNames)), length(cArmNames))
  ) {
  
  new_lInitIntr(
    
    fnInitIntr = function(lPltfTrial, lAddArgs) {
        
      isa <- 
        list(
          # assign ID depending on how many ISAs are already in platform
          nID          = length(lPltfTrial$isa) + 1, 
          # get the ISA name
          cIntrName    = lAddArgs$cIntrName, 
          # get the arm names
          cArmNames    = lAddArgs$cArmNames, 
          # is ISA currently enrolling
          bEnrl        = TRUE, 
          # What is the maximum sample size of this ISA
          nMaxNIntr    = lAddArgs$nMaxNIntr,
          # at what calendar time was ISA started
          nStartTime   = lPltfTrial$lSnap$dCurrTime, 
          # at what calendar time was enrollment to ISA stopped
          nEndEnrlTime = Inf,
          # at what calendar time was decision for ISA made
          nEndTime     = NA, 
          # reason why ISA was stopped
          cEndReason   = NA,
          # List of Future Randomizations
          vRandList    = lAddArgs$vRandList,
          # List of conducted analyses
          lAnalyses    = list(),
          # List of patients in this ISA
          lPats       = list(),
          # List of arms within this ISA
          lArms        = vector("list", length(lAddArgs$cArmNames))
        )
      
      for (i in 1:length(lAddArgs$cArmNames)) {
        isa$lArms[[i]]$cArmName <- lAddArgs$cArmNames[i]
        isa$lArms[[i]]$bEnrl   <- TRUE
        isa$lArms[[i]]$nMaxNArm <- lAddArgs$vMaxNArms[i]
      }
      
      return(isa)
      
    },
    
    lAddArgs   = list(
      cIntrName = cIntrName, 
      cArmNames = cArmNames,
      nMaxNIntr = nMaxNIntr,
      vRandList = vRandList,
      vMaxNArms = vMaxNArms
    )
  )
  
}
