#' Creates a simple platform trial design of class lPltfDsgn
#'
#' @param n_fin                Sample size per cohort at final
#'
#' @param cohorts_start        Number of cohorts to start the platform with
#'
#' @param rr_comb              Response rates of treatment
#'
#' @param rr_plac              Response rate of the SoC
#'
#' @param cohort_fixed         If not NULL, fixed timesteps after which a cohort will be included
#'
#' @param cohorts_max          Maximum number of cohorts that are allowed to be added throughout the trial
#' 
#' @param time_trend           Change in response rate over time
#'
#' @param sharing_type         Which control data should be shared. Default is "All", meaning all available data will be used for analysis. Other options include "NCC", which will use NCC data for evidence levels 2&3. Finally, "CC" will use only concurrent control data for all levels of evidence.
#' 
#' @param accrual_param        Number of patients per week
#' 
#' @param Bayes_Sup            Efficacy Decision Rules 
#'
#' @param ...                  Further arguments to be passed to decision function, such as decision making criteria
#'
#'
#' @return Object of class lPltfDsgn to be supplied to fnRunSingleTrialSim or modified later
#'
#' @examples
#'
#' # Comparison IA1
#' Bayes_Sup <- matrix(nrow = 3, ncol = 2)
#' Bayes_Sup[1,] <- c(0.00, 0.95)
#' Bayes_Sup[2,] <- c(0.10, 0.80)
#' Bayes_Sup[3,] <- c(0.20, 0.60)
#' 
#' lPltfDsgn <- fnWP6NCC(Bayes_Sup = Bayes_Sup)
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn)
#' ocs1 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 20)
#' 
#' @export
fnWP6NCC <- function(
    n_fin = 150,
    cohorts_start = 2,
    rr_comb  = 0.35,
    rr_plac  = 0.1,
    cohort_fixed = 24,
    cohorts_max  = 5,
    sharing_type = "All",
    accrual_param = 6,
    time_trend = 0,
    Bayes_Sup = matrix(c(0, 0.95, 0.05, 0.80, 0.10, 0.60), nrow = 3, ncol = 2, byrow = TRUE)
) {
  
  # Create all sort of warning messages and stop messages
  
  lAnls_WP6NCC <- function(group1, group2, Bayes_Sup = NULL) {
    new_lAnls(
      fnAnls = function(lPltfTrial, lAddArgs) {
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        # and that under "nMstn" we will find the number of milestone that was reached
        
        # Helper function posterior probability of one Beta being by margin delta greater than other Beta
        post_prob_bin <- function(n_exp, n_contr, resp_exp, resp_contr, delta,
                                  a0_exp, b0_exp, a0_contr, b0_contr) {
          
          # in notation of diploma thesis, this calculates the probability P(P_e >= P_c + delta_sup)
          prob_sup <- stats::integrate(function(y) {
            stats::dbeta(y, a0_exp + resp_exp, b0_exp - resp_exp + n_exp) *
              stats::pbeta(y - delta, a0_contr + resp_contr, b0_contr - resp_contr + n_contr)
          }, delta, 1)$value
          
          # return posterior probability
          return(prob_sup)
        }
        
        # Set beta prior
        beta_prior <- 0.5
        
        # For each group, decide which data is going to be used 
        # ("All" is just pooling, "Conc" uses only concurrent data and "Intr" uses only within Intr data)
        
        # Firstly create dataset for analysis
        # Do separately for group1 and group2 because possibly different data sharing
        # Group1 CC Data
        group1df <- 
          subset(
            do.call(
              rbind.data.frame, 
              lPltfTrial$isa[[lAddArgs$current_id]]$lPats
            ),
            Arm == lAddArgs$group1[1]
          )
          
          # Firstly create all data
          # In each ISA get only columns that are already in group1df
          col_names <- colnames(group1df)
          intr_indices <- 1:length(lPltfTrial$isa)
          # Get only out of ISA indices
          intr_indices <- intr_indices[!intr_indices == lAddArgs$current_id]
          outside_data <- list()
          for (i in intr_indices) {
            # Make sure that these ISAs are not empty (otherwise error is thrown)
            if (length(lPltfTrial$isa[[i]]$lPats) > 0) {
              outside_data[[i]] <- 
                subset(
                  do.call(
                    rbind.data.frame, 
                    lPltfTrial$isa[[i]]$lPats
                  ),
                  Arm == lAddArgs$group1[1],
                  select = col_names
                )
            }
          }
          
          # Create Dataset (merge)
          group1df_outside_intr <- 
            do.call(
              plyr::rbind.fill,
              outside_data
            )
            
            later <- 
              which(
                group1df_outside_intr$InclusionTime >= 
                  lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime
              )
            earlier <- 
              which(
                group1df_outside_intr$InclusionTime <= 
                  lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime
              )
            
            group1df_outside_intr_CC <- 
              group1df_outside_intr[intersect(later, earlier), ]

          
          # create final datasets
          group1dfCC <- 
            plyr::rbind.fill(group1df, group1df_outside_intr_CC)
          
          group1dfNCC <- 
            plyr::rbind.fill(group1df, group1df_outside_intr)

        
        # make sure that outcomes observed
        group1dfCC <- 
          group1dfCC %>% 
          dplyr::filter(
            OutObsTime <= lPltfTrial$lSnap$dCurrTime
          )
        
        group1dfNCC <- 
          group1dfNCC %>% 
          dplyr::filter(
            OutObsTime <= lPltfTrial$lSnap$dCurrTime
          )
        
        # Group2 Data
        group2df <- 
          subset(
            do.call(
              rbind.data.frame, 
              lPltfTrial$isa[[lAddArgs$current_id]]$lPats
            ),
            Arm == lAddArgs$group2[1]
          )
        
        # Add data from other cohorts if necessary
        if (lAddArgs$group2[2] != "Intr") {
          
          # Firstly create all data
          # In each ISA get only columns that are already in group1df
          col_names <- colnames(group2df)
          intr_indices <- 1:length(lPltfTrial$isa)
          # Get only out of ISA indices
          intr_indices <- intr_indices[!intr_indices == lAddArgs$current_id]
          outside_data <- list()
          for (i in intr_indices) {
            # Make sure that these ISAs are not empty (otherwise error is thrown)
            if (length(lPltfTrial$isa[[i]]$lPats) > 0) {
              outside_data[[i]] <- 
                subset(
                  do.call(
                    rbind.data.frame, 
                    lPltfTrial$isa[[i]]$lPats
                  ),
                  Arm == lAddArgs$group2[1],
                  select = col_names
                )
            }
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
            
            later <- 
              which(
                group2df_outside_intr$InclusionTime >= 
                  lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime
              )
            earlier <- 
              which(
                group2df_outside_intr$InclusionTime <= 
                  lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime
              )
            
            group2df_outside_intr <- 
              group2df_outside_intr[intersect(later, earlier), ]
            
            # group2df_outside_intr <- 
            #   subset(
            #     group2df_outside_intr,
            #     InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime,
            #     InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime
            #   )
            
          }
          
          # create final dataset
          group2df <- 
            plyr::rbind.fill(group2df, group2df_outside_intr)
          
        }
        
        # make sure that outcomes observed
        group2df <- 
          group2df %>% 
          dplyr::filter(
            OutObsTime <= lPltfTrial$lSnap$dCurrTime
          )
        
        # Combine Datasets
        analysis_data <- 
          rbind(
            group1df,
            group2df
          )
        
        # Create vector of sample sizes and responders for both outcomes
        n_tot1CC <- sum(!is.na(group1dfCC$Outcome))

        n_tot1NCC <- sum(!is.na(group1dfNCC$Outcome))
        
        n_tot2 <- sum(!is.na(group2df$Outcome))
        
        resp_tot1CC <- sum(group1dfCC$Outcome == 1, na.rm = TRUE)
        
        resp_tot1NCC <- sum(group1dfNCC$Outcome == 1, na.rm = TRUE)
        
        resp_tot2 <- sum(group2df$Outcome == 1, na.rm = TRUE)
        
        # return List is split into two sublists: Outcome 1 and Outcome 2
        # Within each sublist, there are two sublists: Efficacy and Futility
        # Within each of these sublists, we find all the posterior probabilities 
        # corresponding to the rows in Bayes_Sup and Bayes_Fut
          
          # Get the decision rule for interim/final
          Bayes_Sup <- lAddArgs$Bayes_Sup
          
          sup_reached <- rep(NA, nrow(Bayes_Sup))
          prob_sup_vec <- rep(NA, nrow(Bayes_Sup))
          
          for (i in 1:(nrow(Bayes_Sup))) {
            
            if (lAddArgs$sharing_type == "All") {
              n_tot1 <- n_tot1NCC
              resp_tot1 <- resp_tot1NCC
            } else if(lAddArgs$sharing_type == "CC") {
              n_tot1 <- n_tot1CC
              resp_tot1 <- resp_tot1CC
            } else {
              
              if (i == 1) {
                n_tot1 <- n_tot1CC
                resp_tot1 <- resp_tot1CC
              } else {
                n_tot1 <- n_tot1NCC
                resp_tot1 <- resp_tot1NCC
              }
            }
            
            # Evaluate decision rule, but if non-existent, give NA
            if (is.na(Bayes_Sup[i,1])) {
              prob_sup_vec[i] <- NA
            } else {
              prob_sup_vec[i] <-
                post_prob_bin(
                  n_tot2, n_tot1,
                  resp_tot2, resp_tot1,
                  Bayes_Sup[i,1], 
                  beta_prior, beta_prior, beta_prior, beta_prior
                )
            }
            
            sup_reached[i] <- prob_sup_vec[i] > Bayes_Sup[i,2]
            
          }
          
          outcome_sup <- list(sup_reached = all(sup_reached),  prob_sup_vec = prob_sup_vec)
        
        
        results <- 
          list(
            outcome_sup = outcome_sup
          )
        
        # Save Analysis Dataset as well
        lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses <- 
          list(
            results = results,
            analysis_data = analysis_data
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
      lAddArgs     = list(
        group1     = group1, 
        group2     = group2,
        Bayes_Sup  = Bayes_Sup,
        sharing_type = sharing_type
      )
    )
    
  }
  
  
  lSynthRes_WP6NCC <- function() {
    
    new_lSynthRes(
      fnSynthRes = function(lPltfTrial, lAddArgs) {
        
        if (length(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses) > 0) {
          
          # if any of the two endpoints is successful, declare success
          if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses$results$outcome_sup$sup_reached) {
            lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Efficacy"
          } else {
            lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility" 
          } 
          
          lPltfTrial$isa[[lAddArgs$current_id]]$nEndTime <- lPltfTrial$lSnap$dCurrTime
          
          print(
            paste0(
              "For ISA ",
              lAddArgs$current_id,
              " ",
              lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason,
              " has been declared at time ",
              lPltfTrial$lSnap$dCurrTime
            )
          )
          
        }
        
        return(lPltfTrial)
        
      },
      lAddArgs   = list()
    )
  }
  
  # add all ISAs separately
  lIntrDsgn <- list()
  for (i in 1:cohorts_max) {
    lIntrDsgn[[i]] <- 
      list(
        lInitIntr       = lInitIntr(cIntrName = paste0("ISA", i), cArmNames = c("C", "T"), nMaxNIntr = n_fin),
        lAllocArm       = lAllocArm(),
        lPatOutcome     = 
          lPatOutcome(
            cGroups = c("C", "T"), 
            dTheta = c(rr_plac, rr_comb), 
            dSigma = NULL, 
            dTrend = time_trend, 
            nLag = 0
          ),
        lCheckAnlsMstn  = lCheckAnlsMstn(vInfTimes = c(1), column = "OutObsTime"),
        lAnls           = 
          lAnls_WP6NCC(
            group1 = c("C", sharing_type), 
            group2 = c("T", "Intr"),
            Bayes_Sup = Bayes_Sup
          ),
        lSynthRes       = lSynthRes_WP6NCC(),
        lCheckEnrl      = lCheckEnrl()
      )
  }
  
  lPltfSummary_WP6NCC <- 
    new_lPltfSummary(
      
      # By default do nothing extra
      fnPltfSummary = function(lPltfTrial, lAddArgs) {
        
        # Define truth via:
        # Is RR1 > RR2
        
        truth <- rep(lAddArgs$rr_comb > lAddArgs$rr_plac, length(lPltfTrial$isa))
        
        # Check which decisions were correct positives, false positives, etc.
        cp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" &  truth)
        fp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" & !truth)
        cn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" & !truth)
        fn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" &  truth)
        
        # Prepare return list
        ret <- list(
          Decision               = sapply(lPltfTrial$isa, function(x) x$cEndReason),
          Start_Time             = sapply(lPltfTrial$isa, function(x) x$nStartTime),
          RR_Comb                = lAddArgs$rr_comb,
          RR_Plac                = lAddArgs$rr_plac,
          N_Cohorts              = length(lPltfTrial$isa),
          # This gets all allocated patients (more patients can be allocated than have outcomes observed)
          ISA_N_Alloc            = sapply(
            lPltfTrial$isa, 
            function(x) nrow(do.call(rbind.data.frame, x$lPats))
          ),
          # This gets all patients with observed outcomes
          ISA_N_Obs              = sapply(
            lPltfTrial$isa, 
            function(x) nrow(subset(do.call(rbind.data.frame, x$lPats), !is.na(OutObsTime)))
          ),
          
          Total_N_Alloc          = sum(sapply(
            lPltfTrial$isa, 
            function(x) nrow(do.call(rbind.data.frame, x$lPats))
          ), na.rm = TRUE),
          Total_N_Obs            = sum(sapply(
            lPltfTrial$isa, 
            function(x) nrow(subset(do.call(rbind.data.frame, x$lPats), !is.na(OutObsTime)))
          ), na.rm = TRUE),
          Total_Time             = lPltfTrial$lSnap$dCurrTime,
          TP                     = cp,
          FP                     = fp,
          TN                     = cn,
          FN                     = fn,
          FDR_Trial              = ifelse(!is.na(fp/(cp + fp)), fp/(cp + fp), NA),
          PTP_Trial              = ifelse(!is.na(cp/(cp + fn)), cp/(cp + fn), NA),
          PTT1ER_Trial           = ifelse(!is.na(fp/(fp + cn)), fp/(fp + cn), NA),
          any_P                  = as.numeric((cp + fp) > 0)
        )
        
        return(ret)
        
      },
      # Pass true response rates to summary function
      lAddArgs   = list(
        rr_comb = rr_comb,
        rr_plac = rr_plac
      )
    )
  
  
  lOCSynth_WP6NCC <- 
    new_lOCSynth(
      
      fnOCSynth = function(lIndTrials, lAddArgs) {
        
        lOCs <- 
          list(
            
            # use as.matrix in case only one treatment is in the trial to avoid matrix to vector conversion
            
            ISA_Total_Time     = mean(sapply(lIndTrials, function(x) x$Total_Time)),
            ISA_Entry_Time     = rowMeans(sapply(lIndTrials, function(x) x$Start_Time)),
            
            ISA_N_Obs          = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Obs)),
            ISA_N_Alloc        = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Alloc)),
            
            Avg_N_Trial_Obs    = mean(sapply(lIndTrials, function(x) x$Total_N_Obs)),
            Avg_N_Trial_Alloc  = mean(sapply(lIndTrials, function(x) x$Total_N_Alloc)),
            
            Avg_Time           = mean(sapply(lIndTrials, function(x) x$Total_Time)),
            
            
            Avg_TP                     = mean(sapply(lIndTrials, function(x) x$TP)),
            Avg_FP                     = mean(sapply(lIndTrials, function(x) x$FP)),
            Avg_TN                     = mean(sapply(lIndTrials, function(x) x$TN)),
            Avg_FN                     = mean(sapply(lIndTrials, function(x) x$FN)),
            Avg_any_P                  = mean(sapply(lIndTrials, function(x) x$any_P)),
            
            
            FDR                        = sum(sapply(lIndTrials, function(x) x$FP)) /
              sum(sapply(lIndTrials, function(x) x$TP) + sapply(lIndTrials, function(x) x$FP)),
            PTP                        = sum(sapply(lIndTrials, function(x) x$TP)) /
              sum((sapply(lIndTrials, function(x) x$TP) + sapply(lIndTrials, function(x) x$FN))),
            PTT1ER                     = sum(sapply(lIndTrials, function(x) x$FP)) /
              sum((sapply(lIndTrials, function(x) x$FP) + sapply(lIndTrials, function(x) x$TN)))
            
          )
        
        return(lOCs)
        
      },
      
      lAddArgs   = list()
      
    )
  
    
  lNewIntr_WP6NCC <- 
    new_lNewIntr(
      fnNewIntr  = function(lPltfTrial, lAddArgs) {
        if (lPltfTrial$lSnap$dCurrTime == 1) {
          dAdd <- lAddArgs$cohorts_start
          # if it has been 4 time units since last inclusion, add one ISA
        } else if (lPltfTrial$lSnap$dCurrTime == max(lPltfTrial$lSnap$vIntrInclTimes) + lAddArgs$nTimeDiff & 
                   length(lPltfTrial$lSnap$vIntrInclTimes) < lAddArgs$nMaxIntr) {
          dAdd <- 1
        } else {
          dAdd <- 0
        }
        return(dAdd)
      },
      lAddArgs      = list(nTimeDiff = cohort_fixed, nMaxIntr = cohorts_max, cohorts_start = cohorts_start)
    )

  
  lPltfDsgn <- 
    lPltfDsgn(
      lAddIntr      = lAddIntr(),
      lAddPats      = lAddPats(),
      lAllocIntr    = lAllocIntr(),
      lIntrDsgn     = lIntrDsgn,
      lNewIntr      = lNewIntr_WP6NCC,
      lOCSynth      = lOCSynth_WP6NCC,
      lPltfSummary  = lPltfSummary_WP6NCC,
      lRecrPars     = lRecrPars(accrual_param),
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc()
    )
  
  return(lPltfDsgn)
  
}
