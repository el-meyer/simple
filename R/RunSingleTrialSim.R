#' @export

lPltfDsgn <- list()

# Get Function Definitions from modular functions
fnGenNewPatData  <- match.fun(lFnDef$fnGenNewPatData)
fnAddNewIntr     <- match.fun(lFnDef$fnAddNewIntr)

# For Starting ISAs work with indicator in lIntrDsgn

# while (trial_is_running) {
  
  dCurrTime <- NA
  dActvIntr <- NA
  
  # Run Generate New Patient Data function
  fnGenNewPatData(
    lPltfTrial,
    lPltfDsgn, 
    dCurrTime
  )
  
  # Run Add New ISAs function
  fnAddNewIntr(
    lPltfTrial,
    lPltfDsgn, 
    dCurrTime
  )
  
# }
