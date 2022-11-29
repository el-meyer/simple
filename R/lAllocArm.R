#' Within ISA allocation ratio
#' 
#' Functions and rules for within ISA allocation ratio of class lAllocArm
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lAllocArm
#' 
#' @export
#' @rdname lAllocArm
# Constructor Function
new_lAllocArm <- function(
  # Function that is used in updating within ISA allocation ratios
  fnAllocArm = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAllocArm function
  lAddArgs = list()
) {
  structure(
    list(
      fnAllocArm  = fnAllocArm,
      lAddArgs    = lAddArgs
    ),
    class         = "lAllocArm"
  )
}
#' @export
#' @rdname lAllocArm
# Validator Function
validate_lAllocArm <- function(x) {
  
}
#' @export
#' @rdname lAllocArm
# Helper Function
lAllocArm <- function() {
  
  new_lAllocArm(
    fnAllocArm = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      
      # By default just use values from isa$vRandList
      
      # What to do if not enough pre-randomizations?
      if (
        nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats) > 
        length(lPltfTrial$isa[[lAddArgs$current_id]]$vRandList)
        ) {
        
        # What to do if no randomizations left?
        if (length(lPltfTrial$isa[[lAddArgs$current_id]]$vRandList) == 0) {
          arms <-             
            rep(
              "NA", 
              nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)
          )
        } else {
          # at least randomization left
          
          arms <-           
            c(
              lPltfTrial$isa[[lAddArgs$current_id]]$vRandList[1:length(lPltfTrial$isa[[lAddArgs$current_id]]$vRandList)],
              rep(
                "NA", 
                nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats) - 
                  length(lPltfTrial$isa[[lAddArgs$current_id]]$vRandList)
              )
            )
          
          # Remove randomizations from vRandList
          lPltfTrial$isa[[lAddArgs$current_id]]$vRandList <- 
            lPltfTrial$isa[[lAddArgs$current_id]]$vRandList[-(1:length(lPltfTrial$isa[[lAddArgs$current_id]]$vRandList))]
          
        }
        
        # Assign Arm Names from vRandList
        lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Arm <- arms
        
        print(
          paste0(
            "Not all or no patients were allocated within ISA ", 
            lAddArgs$current_id,
            " at time ",
            lPltfTrial$lSnap$dCurrTime,
            " because randomization list is empty (e.g. maximum sample size reached)."
          )
        )
        
      } else {
        
        # Assign Arm Names from vRandList
        lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Arm <- 
          lPltfTrial$isa[[lAddArgs$current_id]]$vRandList[1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)]
       
        # Remove randomizations from vRandList
        lPltfTrial$isa[[lAddArgs$current_id]]$vRandList <- 
          lPltfTrial$isa[[lAddArgs$current_id]]$vRandList[-(1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats))]
         
      }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
}
