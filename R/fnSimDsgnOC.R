#' Calculates the operating characteristics of a platfom design
#'
#' @param lPltfDsgn    Platform Design List
#'
#' @param nIter        Number of iterations to run
#' 
#' @param nCores       Number of cores to run on
#' 
#' @param bSaveCsv     Indicator whether simulation results should be saved in a CSV file
#'
#' @param cPathCsv     Path to which simulation results will be saved; if NULL, then save to current path
#' 
#' @param cNameCsv     Filename for CSV File to be saved as
#'
#' @param bRetList     Indicator whether function should save list of results
#'
#' @param bRetInd      Indicator whether individual trial results should be saved as well
#' 
#' @param export       Further Arguments to be exported to slaves
#'
#' @param ...          All other design parameters 
#'
#' @return List of operating characteristics
#'
#' @examples
#'
#' ocs <- fnSimDsgnOC(lPltfDsgn = fnSimpleDesign(), nIter = 3)
#'
#' @export

# Where to add data management to reduce runtime?

fnSimDsgnOC <-
  function(
    lPltfDsgn,                
    nIter,
    nCores   = 1,
    bSaveCsv = FALSE,
    cPathCsv = getwd(),
    cNameCsv = "Test",
    bRetList = FALSE,
    bRetInd  = FALSE,
    export = NULL,
    ...
  ) {
    
    # Always use lSummary
    
    
    # Since R CMD check allows only for 2 cores, set this
    if (nCores > 1) {
      chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(chk) && chk == "TRUE") {
        nCores <- 2
      }
      
      # Prepare for parallel computing
      cl <- parallel::makePSOCKcluster(nCores)
      doParallel::registerDoParallel(cl)
      
      "%dopar%" <- foreach::"%dopar%"
      
      arguments <- list(lPltfDsgn, bRetainSnaps = FALSE, ...) # gather additional program arguments
      
      ##### Run parallel simulations #####
      
      # run in parallel
      trial_results <- foreach::foreach(i = 1:nIter, .packages = "simple", .export = export) %dopar% {
        # first call program function
        trial_res <- do.call(fnRunSingleTrialSim, arguments)
        # Now save individual trial results
        trial_res$lSummary
      }
      # end parallel
      doParallel::stopImplicitCluster()
      # closeAllConnections()
      parallel::stopCluster(cl)
      
    } else {
      
      arguments <- list(lPltfDsgn, ...) # gather additional program arguments
      
      ##### Run parallel simulations #####
      
      # run without parallel
      trial_results <- list()
      
      for (i in 1:nIter) {
        # first call program function
        trial_res <- do.call(fnRunSingleTrialSim, arguments)
        # Now save individual trial results
        trial_results <- c(trial_results, list(trial_res$lSummary))
      }
      
    }
    
    
    assign(
      "lOC",
      do.call(
        match.fun(lPltfDsgn$lOCSynth$fnOCSynth),
        args = list(
          lIndTrials = trial_results
        )
      )
    )
    
    
    # Get all function arguments to display later
    arguments_full <- list(iter = nIter, nCores = nCores, ...)
    
    
    # ##### Save as Excel and RData #####
    # 
    # if (bSaveCsv) {
    #   # If results should be saved, save to Excel and slightly recode ret1 to be a matrix
    #   # If path is supplied, results will be saved in folder with program name at this path; if folder with program names does not exist, create one
    #   # If path is not supplied, go to current WD, create folder "temp" and proceed analogously
    #   
    #   # Get the above results (which are a list) and convert to 1xk vector for better display in excel file
    #   ret2 <- t(as.matrix(ret1))
    #   # Create return object which includes a sheet (==list element) with all the design parameters,
    #   # the program OCs and all the program simulations results
    #   
    #   arguments_full2 <- unlist(arguments_full)
    #   arguments_full2[which(sapply(arguments_full2, function(x) is.function(x)))] <-
    #     as.character(arguments_full2[which(sapply(arguments_full2, function(x) is.function(x)))])
    #   ret <- list(t(arguments_full2), ret2)
    #   
    #   # Additionall check whether Unix (Mac, Linux) or not to account for minor differences
    #   if (.Platform$OS.type == "unix") {
    #     if (is.null(path)) {
    #       path0 <- getwd()
    #       ifelse(!dir.exists(file.path(path0, "tempsim/")), dir.create(file.path(path0, "temp/")), FALSE)
    #       path <- file.path(path0, "tempsim/")
    #     }
    #     
    #     file.savepath <- paste0(path, filename, ".xlsx")
    #     
    #     # Write to xlsx
    #     openxlsx::write.xlsx(ret, file = file.savepath)
    #     # Save as RData
    #     file.savepath.rdata <- paste0(path, filename, ".RData")
    #     results <- c(list(arguments_full), list(ret1), trial_results)
    #     save(results, file = file.savepath.rdata)
    #     
    #   } else {
    #     if (is.null(path)) {
    #       path0 <- getwd()
    #       ifelse(!dir.exists(file.path(path0, "tempsim")), dir.create(file.path(path0, "tempsim")), FALSE)
    #       path <- file.path(path0, "tempsim")
    #     }
    #     
    #     file.savepath <- paste0(path, "/", filename, ".xlsx")
    #     
    #     # Write to xlsx
    #     openxlsx::write.xlsx(ret, file = file.savepath)
    #     # Save as RData
    #     file.savepath.rdata <- paste0(path, "/", filename, ".RData")
    #     results <- c(list(arguments_full), list(ret1), trial_results)
    #     save(results, file = file.savepath.rdata)
    #   }
    # }
    
    
    # Return return list
    return(
      lOC
    )
    
  }
