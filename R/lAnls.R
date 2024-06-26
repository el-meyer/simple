#' Run Analysis
#' 
#' Functions and rules for conducting analyses of class lAnls
#' 
#' @param 
#' 
#' @examples
#' 
#' @name lAnls
#' 
#' @export
#' @rdname lAnls
# Constructor Function
new_lAnls <- function(
  # Function that is used in updating between ISA allocation ratios
  fnAnls = function(
    lPltfTrial, # List of current platform trial progress
    lAddArgs    # List of further arguments for this module
  ) {}, 
  # List of Arguments used with fnAnls function
  lAddArgs = list()
) {
  structure(
    list(
      fnAnls    = fnAnls,
      lAddArgs  = lAddArgs
    ),
    class       = "lAnls"
  )
}
#' @export
#' @rdname lAnls
# Validator Function
validate_lAnls <- function(x) {
  
}
#' @export
#' @rdname lAnls
# Helper Function
lAnls <- function(
  endpoint = "binary",
  analysis_function_binary = function(x) {
    matOutcome <- table(x$Arm, x$Outcome)
    if (ncol(matOutcome) == 1) {
      ret <- 1
    } else (
      ret <- 
        stats::prop.test(
          matOutcome
        )$p.value
    )
    return(ret)
  }, # takes whole dataset as input
  analysis_function_continuous = function(x) {
    stats::t.test(
      Outcome ~ Arm,
      data = x
    )$p.value
  }, # takes whole dataset as input
  group1 = c("C", "All"), 
  group2
) {
  
  new_lAnls(
    fnAnls = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
      # and that under "nMstn" we will find the number of milestone that was reached
      
      # By default, regardless of which Analysis Milestone, just run the same analysis
      # If endpoint == "binary", run Chi-Square Test
      # If endpoint == "continuous", run T-Test
      
      # Get correct analysis function
      if (lAddArgs$endpoint == "binary") {
        
        analysis_function <- lAddArgs$analysis_function_binary
        
      } else if (lAddArgs$endpoint == "continuous") {
        
        analysis_function <- lAddArgs$analysis_function_continuous
        
      } else {
        
        stop("lAnls misspecified.")
        
      }
    
      # For each group, decide which data is going to be used 
      # ("All" is just pooling, "Conc" uses only concurrent data and "Intr" uses only within Intr data)
      
      # Firstly create dataset for analysis
      # Do separately for group1 and group2 because possibly different data sharing
      # Group1 Data
      
      temp_dat <- 
        do.call(
          rbind.data.frame, 
          lPltfTrial$isa[[lAddArgs$current_id]]$lPats
        )
      
      cor_arm <- which(temp_dat$Arm == lAddArgs$group1[1])
      
      group1df <- temp_dat[cor_arm,]
        
      # Add data from other cohorts if necessary
      if (lAddArgs$group1[2] != "Intr") {
        
        # Firstly create all data
        intr_indices <- 1:length(lPltfTrial$isa)
        # Get only out of ISA indices
        intr_indices <- intr_indices[!intr_indices == lAddArgs$current_id]
        outside_data <- list()
        for (i in intr_indices) {
          
          temp_dat <- 
            do.call(
              rbind.data.frame, 
              lPltfTrial$isa[[i]]$lPats
            )
          
          cor_arm <- 
            which(temp_dat$Arm == lAddArgs$group1[1])
          
          outside_data[[i]] <- temp_dat[cor_arm, ]
        }
        
        # Create Dataset (merge)
        group1df_outside_intr <- 
          do.call(
            plyr::rbind.fill,
            outside_data
          )
        
        # Depending on type of data sharing, filter data
        if (lAddArgs$group1[2] == "Conc") {
          
          # Definition concurrent data: Patients that would have had to possibility to be randomized
          # to arm under investigation
          
          later <- 
            which(group1df_outside_intr$InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime)
          earlier <- 
            which(group1df_outside_intr$InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime)
          
          group1df_outside_intr <- 
            group1df_outside_intr[intersect(later, earlier), ]
          
        }
        
        # create final dataset
        group1df <- 
          plyr::rbind.fill(group1df, group1df_outside_intr)
        
      }
      
      # Group2 Data
      
      temp_dat <- 
        do.call(
          rbind.data.frame, 
          lPltfTrial$isa[[lAddArgs$current_id]]$lPats
        )
      
      cor_arm <- which(temp_dat$Arm == lAddArgs$group2[1])
      
      group2df <- temp_dat[cor_arm,]
      
      # Add data from other cohorts if necessary
      if (lAddArgs$group2[2] != "Intr") {
        
        # Firstly create all data
        intr_indices <- 1:length(lPltfTrial$isa)
        # Get only out of ISA indices
        intr_indices <- intr_indices[!intr_indices == lAddArgs$current_id]
        outside_data <- list()
        for (i in intr_indices) {
          
          temp_dat <- 
            do.call(
              rbind.data.frame, 
              lPltfTrial$isa[[i]]$lPats
            )
          
          cor_arm <- 
            which(temp_dat$Arm == lAddArgs$group2[1])
          
          outside_data[[i]] <- temp_dat[cor_arm, ]
          
        }
        
        # Create Dataset (merge)
        group2df_outside_intr <- 
          do.call(
            plyr::rbind.fill,
            outside_data
          )
        
        # Depending on type of data sharing, filter data
        if (lAddArgs$group2[2] == "Conc") {
          
          # Definition concurrent data: Patients that would have had to possibility to be randomized
          # to arm under investigation
          # 
          
          # filter to only concurrent control data
          
          later <- 
            which(group2df_outside_intr$InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime)
          earlier <- 
            which(group2df_outside_intr$InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime)
          
          group2df_outside_intr <- 
            group2df_outside_intr[intersect(later, earlier), ]
          
        }
        
        # create final dataset
        group2df <- 
          plyr::rbind.fill(group2df, group2df_outside_intr)
        
      }
      
      # Combine Datasets
      analysis_data <- 
        rbind(
          group1df,
          group2df
        ) %>% 
        dplyr::filter(
          OutObsTime <= lPltfTrial$lSnap$dCurrTime
        ) %>% 
        dplyr::mutate(
          Arm = factor(Arm)
        )
      
      results <- match.fun(analysis_function)(analysis_data)
      
      # Save Analysis Dataset and Timing as well
      lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[lAddArgs$nMstn]] <- 
        list(
          results = results,
          analysis_data = analysis_data,
          analysis_time = lPltfTrial$lSnap$dCurrTime
        )
      
      print(
        paste0(
          "An analysis for ISA ",
          lAddArgs$current_id,
          " was conducted at time ",
          lPltfTrial$lSnap$dCurrTime
        )
      )
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list(
      endpoint                     = endpoint,
      analysis_function_binary     = analysis_function_binary, 
      analysis_function_continuous = analysis_function_continuous,
      group1                       = group1, 
      group2                       = group2
    )
  )
  
}
