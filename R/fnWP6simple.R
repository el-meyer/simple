#' Creates a simple platform trial design of class lPltfDsgn
#'
#' @param n_fin                Sample size per cohort at final
#'
#' @param cohorts_start        Number of cohorts to start the platform with
#'
#' @param rr_comb1             Response rates of treatment, histology endpoint 1
#'
#' @param rr_comb2             Response rates of treatment, histology endpoint 2
#'
#' @param rr_plac1             Response rate of the SoC, histology endpoint 1
#'
#' @param rr_plac2             Response rate of the SoC, histology endpoint 2
#'
#' @param correlation          Correlation between histology endpoints
#'
#' @param cohort_fixed         If not NULL, fixed timesteps after which a cohort will be included
#'
#' @param cohorts_max          Maximum number of cohorts that are allowed to be added throughout the trial
#'
#' @param sharing_type         Which backbone and placebo data should be used for arm comparisons; Default is "All".
#'                             Another option is "Conc" or "Intr".
#' 
#' @param accrual_param        Number of patients per week
#'
#' @param analysis_times       Vector of information fractions needed for interim analyses
#'
#' @param hist_lag             Time until histology outcome is observed
#'
#' @param time_trend           Additive term by which response rates increase at every time step
#'
#' @param composite            Rule for deriving the composite endpoint. By default "or", otherwise "and"
#' 
#' @param Bayes_Sup1           Efficacy Rules for Endpoint 1
#' 
#' @param Bayes_Sup2           Efficacy Rules for Endpoint 2
#'
#' @param Bayes_Fut1           Futility Rules for Endpoint 1
#' 
#' @param Bayes_Fut2           Futility Rules for Endpoint 2
#'
#' @param ...                  Further arguments to be passed to decision function, such as decision making criteria
#'
#'
#' @return Object of class lPltfDsgn to be supplied to fnRunSingleTrialSim or modified later
#'
#' @examples
#'
#' # Comparison IA1
#' Bayes_Sup11 <- matrix(nrow = 2, ncol = 2)
#' Bayes_Sup11[1,] <- c(0.00, 0.95)
#' Bayes_Sup11[2,] <- c(0.10, 0.80)
#' # Comparison IA2
#' Bayes_Sup12 <- matrix(nrow = 2, ncol = 2)
#' Bayes_Sup12[1,] <- c(0.00, 0.95)
#' Bayes_Sup12[2,] <- c(0.10, 0.80)
#' # Comparison IA3
#' Bayes_Sup13 <- matrix(nrow = 2, ncol = 2)
#' Bayes_Sup13[1,] <- c(0.00, 0.95)
#' Bayes_Sup13[2,] <- c(0.10, 0.80)
#'
#' Bayes_Sup1 <- Bayes_Sup2 <- list(Bayes_Sup11, Bayes_Sup12, Bayes_Sup13)
#'
#' # Comparison IA1
#' Bayes_Fut11 <- matrix(nrow = 1, ncol = 2)
#' Bayes_Fut11[1,] <- c(0.00, 0.20)
#' # Comparison IA2
#' Bayes_Fut12 <- matrix(nrow = 1, ncol = 2)
#' Bayes_Fut12[1,] <- c(0.00, 0.30)
#' # Comparison IA3
#' Bayes_Fut13 <- NULL
#' # Endpoint 1+2
#' Bayes_Fut1 <- Bayes_Fut2 <- list(Bayes_Fut11, Bayes_Fut12, Bayes_Fut13)
#' 
#' lPltfDsgn <- 
#' fnWP6simple(
#'  Bayes_Sup1 = Bayes_Sup1,
#'  Bayes_Sup2 = Bayes_Sup2, 
#'  Bayes_Fut1 = Bayes_Fut1,
#'  Bayes_Fut2 = Bayes_Fut2
#' )
#' 
#' out <- fnRunSingleTrialSim(lPltfDsgn)
#' ocs1 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 5)
#' 
#' @export
fnWP6simple <- function(
n_fin = 150,
cohorts_start = 2,
rr_comb1 = 0.2,
rr_comb2 = 0.35,
rr_plac1 = 0.1,
rr_plac2 = 0.25,
correlation = 0.5,
cohort_fixed = 24,
cohorts_max  = 5,
sharing_type = "Conc",
accrual_param = 9,
analysis_times = c(0.5, 0.75, 1),
hist_lag = 48,
time_trend = 0,
composite = "or",
Bayes_Sup1,
Bayes_Sup2, 
Bayes_Fut1,
Bayes_Fut2
) {
  
  # Create all sort of warning messages and stop messages
  
  # Check if length of rr_comb etc are either one or exactly max_cohorts
  
  # Get Vector of response probabilities
  if (length(rr_comb1) == 1) {
    rr_comb1_vec <- rep(rr_comb1, cohorts_max)
    rr_comb2_vec <- rep(rr_comb2, cohorts_max)
    rr_plac1_vec <- rep(rr_plac1, cohorts_max)
    rr_plac2_vec <- rep(rr_plac2, cohorts_max)
  } else {
    rr_comb1_vec <- rr_comb1
    rr_comb2_vec <- rr_comb2
    rr_plac1_vec <- rr_plac1
    rr_plac2_vec <- rr_plac2
  }
  
  # Specify Patient Outcome Simulation Module
  lPatOutcome_CorrBin <- function(
    cGroups, # Group Names to which the Thetas apply
    dTheta1, # Success Probability 1 for cGroups
    dTheta2, # Success Probability 2 for cGroups
    dTrend, # Trend per time unit for dTheta
    dCorr, # Correlation between Endpoints
    nLag # Time lag after which outcome is observed
  ) {
    new_lPatOutcome(
      fnPatOutcome = function(lPltfTrial, lAddArgs) {
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        
        # Helper function to create multinomial distribution
        fun_multnom <- function(rr_short, rr_long, correlation) {
          
          prob11 <-
            as.numeric(
              mvtnorm::pmvnorm(
                upper = c(stats::qnorm(rr_short), stats::qnorm(rr_long)),
                corr = matrix(
                  c(1, correlation,
                    correlation, 1),
                  nrow = 2, ncol = 2
                )
              )
            )
          
          prob01 <-
            as.numeric(
              mvtnorm::pmvnorm(
                lower = c(stats::qnorm(rr_short), -Inf),
                upper = c(Inf, stats::qnorm(rr_long)),
                corr = matrix(
                  c(1, correlation,
                    correlation, 1),
                  nrow = 2, ncol = 2
                )
              )
            )
          
          prob10 <-
            as.numeric(
              mvtnorm::pmvnorm(
                lower = c(-Inf, stats::qnorm(rr_long)),
                upper = c(stats::qnorm(rr_short), Inf),
                corr = matrix(
                  c(1, correlation,
                    correlation, 1),
                  nrow = 2, ncol = 2
                )
              )
            )
          
          
          prob00 <-
            as.numeric(mvtnorm::pmvnorm(
              lower = c(stats::qnorm(rr_short), stats::qnorm(rr_long)),
              upper = c(Inf, Inf),
              corr = matrix(
                c(1, correlation,
                  correlation, 1),
                nrow = 2, ncol = 2
              )
            )
            )
          
          return(
            c(
              p00 = prob00,
              p10 = prob10,
              p01 = prob01,
              p11 = prob11
            )
          )
          
        }
        
        for (i in 1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)) {
          
          # Get the arm of the patient
          index <- 
            which(lAddArgs$cGroups == lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Arm[i])
          
          # If patient was not assigned to an arm, the outcome is NA
          # Otherwise do regular simulations
          
          if (length(index) == 0) {
            
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome1[i] <- NA
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome2[i] <- NA
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- NA
            
          } else {
            
            # Get success probabilities
            prob1 <- 
              max(
                min(
                  lAddArgs$dTheta1[index] + lAddArgs$dTrend * lPltfTrial$lSnap$dCurrTime,
                  1
                ),
                0
              )
            
            # Get success probabilities
            prob2 <- 
              max(
                min(
                  lAddArgs$dTheta2[index] + lAddArgs$dTrend * lPltfTrial$lSnap$dCurrTime,
                  1
                ),
                0
              )
            
            # Get multinominal probabilities
            new_probs <- fun_multnom(prob1, prob2, lAddArgs$dCorr)
            
            # Draw from multinomial distribution
            draw <- t(stats::rmultinom(1, 1, new_probs))
            resp_hist1 <- 0
            resp_hist2 <- 0
            if (draw[1,2] == 1) {
              resp_hist1 <- resp_hist1 + 1
            }
            if (draw[1,3] == 1) {
              resp_hist2 <- resp_hist2 + 1
            }
            if (draw[1,4] == 1) {
              resp_hist1 <- resp_hist1 + 1
              resp_hist2 <- resp_hist2 + 1
            }
            
            # Assign Outcomes
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome1[i] <- resp_hist1
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$Outcome2[i] <- resp_hist2
            
            lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- 
              lPltfTrial$lSnap$dCurrTime + lAddArgs$nLag
            
          }
          
        }
        
        return(lPltfTrial)
        
      },
      lAddArgs   = 
        list(
          cGroups = cGroups, 
          dTheta1 = dTheta1, 
          dTheta2 = dTheta2, 
          dCorr = dCorr, 
          dTrend = dTrend, 
          nLag = nLag
        )
    )
  }
    
    
    lAnls_CorrBin <- function(group1, group2, Bayes_Sup1 = NULL, Bayes_Sup2 = NULL, Bayes_Fut1 = NULL, Bayes_Fut2 = NULL) {
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
          # Group1 Data
          group1df <- 
            subset(
              do.call(
                rbind.data.frame, 
                lPltfTrial$isa[[lAddArgs$current_id]]$lPats
              ),
              Arm == lAddArgs$group1[1]
            )
          
          # Add data from other cohorts if necessary
          if (lAddArgs$group1[2] != "Intr") {
            
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
            
            # Depending on type of data sharing, filter data
            if (lAddArgs$group1[2] == "Conc") {
              
              # Definition concurrent data: Patients that would have had to possibility to be randomized
              # to arm under investigation
              
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
              
              group1df_outside_intr <- 
                group1df_outside_intr[intersect(later, earlier), ]
              
              # group1df_outside_intr <- 
              #   subset(
              #     group1df_outside_intr,
              #     InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime,
              #     InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime
              #   )
              
            }
            
            # create final dataset
            group1df <- 
              plyr::rbind.fill(group1df, group1df_outside_intr) %>% 
              dplyr::filter(
                OutObsTime <= lPltfTrial$lSnap$dCurrTime
              )
            
          }
          
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
              plyr::rbind.fill(group2df, group2df_outside_intr) %>% 
              dplyr::filter(
                OutObsTime <= lPltfTrial$lSnap$dCurrTime
              )
            
          }
          
          # Combine Datasets
          analysis_data <- 
            rbind(
              group1df,
              group2df
            )
          
          # Create vector of sample sizes and responders for both outcomes
          n_tot1 <- 
            c(
              sum(!is.na(group1df$Outcome1)),
              sum(!is.na(group2df$Outcome1))
            )
        
          n_tot2 <- 
            c(
              sum(!is.na(group1df$Outcome2)),
              sum(!is.na(group2df$Outcome2))
            )
          
          resp_tot1 <- 
            c(
              sum(group1df$Outcome1 == 1, na.rm = TRUE),
              sum(group2df$Outcome1 == 1, na.rm = TRUE)
            )
          
          resp_tot2 <- 
            c(
              sum(group1df$Outcome2 == 1, na.rm = TRUE),
              sum(group2df$Outcome2 == 1, na.rm = TRUE)
            )
          
          # return List is split into two sublists: Outcome 1 and Outcome 2
          # Within each sublist, there are two sublists: Efficacy and Futility
          # Within each of these sublists, we find all the posterior probabilities 
          # corresponding to the rows in Bayes_Sup and Bayes_Fut

          if (!is.null(lAddArgs$Bayes_Sup1[[lAddArgs$nMstn]])) {
            
            # Get the decision rule for interim/final
            Bayes_Sup1 <- lAddArgs$Bayes_Sup1[[lAddArgs$nMstn]]
            
            sup_reached <- rep(NA, nrow(Bayes_Sup1))
            prob_sup_vec <- rep(NA, nrow(Bayes_Sup1))
            
            for (i in 1:(nrow(Bayes_Sup1))) {
                
                # Evaluate decision rule, but if non-existent, give NA
                if (is.na(Bayes_Sup1[i,1])) {
                  prob_sup_vec[i] <- NA
                } else {
                  prob_sup_vec[i] <-
                    post_prob_bin(
                      n_tot1[2], n_tot1[1],
                      resp_tot1[2], resp_tot1[1],
                      Bayes_Sup1[i,1], 
                      beta_prior, beta_prior, beta_prior, beta_prior
                    )
                }
                
              sup_reached[i] <- prob_sup_vec[i] > Bayes_Sup1[i,2]
              
            }
            
            outcome1_sup <- list(sup_reached = all(sup_reached),  prob_sup_vec = prob_sup_vec)
            
          } else {
            
            outcome1_sup <- list(sup_reached = FALSE,  prob_sup_vec = NA)
            
          }
          
          
          if (!is.null(lAddArgs$Bayes_Sup2[[lAddArgs$nMstn]])) {
            
            # Get the decision rule for interim/final
            Bayes_Sup2 <- lAddArgs$Bayes_Sup2[[lAddArgs$nMstn]]
            
            sup_reached <- rep(NA, nrow(Bayes_Sup2))
            prob_sup_vec <- rep(NA, nrow(Bayes_Sup2))
            
            for (i in 1:(nrow(Bayes_Sup2))) {
              
              # Evaluate decision rule, but if non-existent, give NA
              if (is.na(Bayes_Sup2[i,1])) {
                prob_sup_vec[i] <- NA
              } else {
                prob_sup_vec[i] <-
                  post_prob_bin(
                    n_tot2[2], n_tot2[1],
                    resp_tot2[2], resp_tot2[1],
                    Bayes_Sup2[i,1], 
                    beta_prior, beta_prior, beta_prior, beta_prior
                  )
              }
              
              sup_reached[i] <- prob_sup_vec[i] > Bayes_Sup2[i,2]
              
            }
            
            outcome2_sup <- list(sup_reached = all(sup_reached), prob_sup_vec = prob_sup_vec)
            
          } else {
            
            outcome2_sup <- list(sup_reached = FALSE,  prob_sup_vec = NA)
            
          }
          
          
          if (!is.null(lAddArgs$Bayes_Fut1[[lAddArgs$nMstn]])) {
            
            # Get the decision rule for interim/final
            Bayes_Fut1 <- lAddArgs$Bayes_Fut1[[lAddArgs$nMstn]]
            
            fut_reached <- rep(NA, nrow(Bayes_Fut1))
            prob_fut_vec <- rep(NA, nrow(Bayes_Fut1))
            
            for (i in 1:(nrow(Bayes_Fut1))) {
              
              # Evaluate decision rule, but if non-existent, give NA
              if (is.na(Bayes_Fut1[i,1])) {
                prob_fut_vec[i] <- NA
              } else {
                prob_fut_vec[i] <-
                  post_prob_bin(
                    n_tot1[2], n_tot1[1],
                    resp_tot1[2], resp_tot1[1],
                    Bayes_Fut1[i,1], 
                    beta_prior, beta_prior, beta_prior, beta_prior
                  )
              }
              
              fut_reached[i] <- prob_fut_vec[i] < Bayes_Fut1[i,2]
              
            }
            
            outcome1_fut <- list(fut_reached = any(fut_reached), prob_fut_vec = prob_fut_vec)
            
          } else {
            
            outcome1_fut <- list(fut_reached = FALSE,  prob_fut_vec = NA)
            
          }
          
          
          if (!is.null(lAddArgs$Bayes_Fut2[[lAddArgs$nMstn]])) {
            
            # Get the decision rule for interim/final
            Bayes_Fut2 <- lAddArgs$Bayes_Fut2[[lAddArgs$nMstn]]
            
            fut_reached <- rep(NA, nrow(Bayes_Fut2))
            prob_fut_vec <- rep(NA, nrow(Bayes_Fut2))
            
            for (i in 1:(nrow(Bayes_Fut2))) {
              
              # Evaluate decision rule, but if non-existent, give NA
              if (is.na(Bayes_Fut2[i,1])) {
                prob_fut_vec[i] <- NA
              } else {
                prob_fut_vec[i] <-
                  post_prob_bin(
                    n_tot2[2], n_tot2[1],
                    resp_tot2[2], resp_tot2[1],
                    Bayes_Fut2[i,1], 
                    beta_prior, beta_prior, beta_prior, beta_prior
                  )
              }
              
              fut_reached[i] <- prob_fut_vec[i] < Bayes_Fut2[i,2]
              
            }
            
            outcome2_fut <- list(fut_reached = any(fut_reached), prob_fut_vec = prob_fut_vec)
            
          } else {
            
            outcome2_fut <- list(fut_reached = FALSE,  prob_fut_vec = NA)
            
          }
          
          
          results <- 
            list(
              outcome1_sup = outcome1_sup, 
              outcome2_sup = outcome2_sup, 
              outcome1_fut = outcome1_fut,
              outcome2_fut = outcome2_fut
            )
          
          # Save Analysis Dataset as well
          lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[lAddArgs$nMstn]] <- 
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
        lAddArgs   = list(
          group1     = group1, 
          group2     = group2,
          Bayes_Sup1 = Bayes_Sup1,
          Bayes_Sup2 = Bayes_Sup2,
          Bayes_Fut1 = Bayes_Fut1,
          Bayes_Fut2 = Bayes_Fut2
        )
      )
      
    }
    
    
    lSynthRes_CorrBin <- function(vAnlsTimes) {
      
      new_lSynthRes(
        fnSynthRes = function(lPltfTrial, lAddArgs) {
          
          nAnls <- length(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses)
          
          # It is expected that in lAddArgs we will find the current ID under "current_id"
          if (nAnls > 0 & is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
            
            
            # Get combined decision
            if (composite == "or") {
              
              # if any of the two endpoints is successful, declare success
              if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome1_sup$sup_reached |
                  lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome2_sup$sup_reached) {
                lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Efficacy"
              } else
                # if both of the two endpoints are futile, declare futility
                if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome1_fut$fut_reached &
                    lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome2_fut$fut_reached) {
                  lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility" 
                } 
            } else if (composite == "and") {
              # opposite of before
              if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome1_sup$sup_reached &
                  lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome2_sup$sup_reached) {
                lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Efficacy"
              } else
                if (lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome1_fut$fut_reached |
                    lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results$outcome2_fut$fut_reached) {
                  lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility"
                } 
            }
            
            if (is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason) & nAnls == length(vAnlsTimes)) {
              lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility"
            }
            
            
            if (!is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
              
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
          lPatOutcome_CorrBin(
            cGroups = c("C", "T"), 
            dTheta1 = c(rr_plac1_vec[1], rr_comb1_vec[1]), 
            dTheta2 = c(rr_plac2_vec[1], rr_comb2_vec[1]), 
            dTrend  = 0, 
            dCorr   = correlation,
            nLag    = hist_lag
          ),
        lCheckAnlsMstn  = lCheckAnlsMstn(vInfTimes = analysis_times, column = "OutObsTime"),
        lAnls           = 
          lAnls_CorrBin(
            group1 = c("C", sharing_type), 
            group2 = c("T", "Intr"),
            Bayes_Sup1 = Bayes_Sup1,
            Bayes_Sup2 = Bayes_Sup2,
            Bayes_Fut1 = Bayes_Fut1,
            Bayes_Fut2 = Bayes_Fut2
          ),
        lSynthRes       = lSynthRes_CorrBin(analysis_times),
        lCheckEnrl      = lCheckEnrl()
      )
  }
  
  lPltfSummary_WP6 <- 
    new_lPltfSummary(
      
      # By default do nothing extra
      fnPltfSummary = function(lPltfTrial, lAddArgs) {
        
        # Define truth via:
        # Is RR1 > RR2
        
        truth <- rep(NA, length(lPltfTrial$isa))

        for (i in 1:length(truth)) {
          if (composite == "or") {
            truth[i] <-
              (lAddArgs$rr_comb1_vec[i] > lAddArgs$rr_plac1_vec[i])|
              (lAddArgs$rr_comb2_vec[i] > lAddArgs$rr_plac2_vec[i])
          } else if (composite == "and") {
            truth[i] <-
              (lAddArgs$rr_comb1_vec[i] > lAddArgs$rr_plac1_vec[i])&
              (lAddArgs$rr_comb2_vec[i] > lAddArgs$rr_plac2_vec[i])
          }
        }

        # Check which decisions were correct positives, false positives, etc.
        cp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" &  truth)
        fp <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Efficacy" & !truth)
        cn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" & !truth)
        fn <- sum(sapply(lPltfTrial$isa, function(x) x$cEndReason) == "Futility" &  truth)

        # Prepare return list
        ret <- list(
          Decision               = sapply(lPltfTrial$isa, function(x) x$cEndReason),
          Start_Time             = sapply(lPltfTrial$isa, function(x) x$nStartTime),
          RR_Comb1               = lAddArgs$rr_comb1_vec,
          RR_Comb2               = lAddArgs$rr_comb2_vec,
          RR_Plac1               = lAddArgs$rr_plac1_vec,
          RR_Plac2               = lAddArgs$rr_plac2_vec,
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
          any_P                  = as.numeric((cp + fp) > 0),

          # Final Decision is decision
          # Length of lAnalyses shows how many analyses were necessary to get this decision
          
          Intx1_GO                = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Efficacy") & (length(x$lAnalyses) == 1)
          ), na.rm = TRUE),
          Intx1_STOP              = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Futility") & (length(x$lAnalyses) == 1)
          ), na.rm = TRUE),
          
          Intx2_GO                = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Efficacy") & (length(x$lAnalyses) == 2)
          ), na.rm = TRUE),
          Intx2_STOP              = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Futility") & (length(x$lAnalyses) == 2)
          ), na.rm = TRUE),
          
          Intx3_GO                = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Efficacy") & (length(x$lAnalyses) == 3)
          ), na.rm = TRUE),
          Intx3_STOP              = sum(sapply(
            lPltfTrial$isa, 
            function(x) (x$cEndReason == "Futility") & (length(x$lAnalyses) == 3)
          ), na.rm = TRUE)
        )

        return(ret)
        
      },
      # Pass true response rates to summary function
      lAddArgs   = list(
        rr_comb1_vec = rr_comb1_vec,
        rr_comb2_vec = rr_comb2_vec,
        rr_plac1_vec = rr_plac1_vec,
        rr_plac2_vec = rr_plac2_vec
      )
    )
  
  
  lOCSynth_WP6 <- 
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
            
            Avg_Intx1_Go                = sum(sapply(lIndTrials, function(x) x$Intx1_GO)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),
            Avg_Intx2_Go                = sum(sapply(lIndTrials, function(x) x$Intx2_GO)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),
            Avg_Intx3_Go                = sum(sapply(lIndTrials, function(x) x$Intx3_GO)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),
            
            Avg_Intx1_Stop              = sum(sapply(lIndTrials, function(x) x$Intx1_STOP)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),
            Avg_Intx2_Stop              = sum(sapply(lIndTrials, function(x) x$Intx2_STOP)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),
            Avg_Intx3_Stop              = sum(sapply(lIndTrials, function(x) x$Intx3_STOP)) /
              sum(sapply(lIndTrials, function(x) x$N_Cohorts)),

            
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
  
  
  lNewIntr_fixed <- 
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
      lNewIntr      = lNewIntr_fixed,
      lOCSynth      = lOCSynth_WP6,
      lPltfSummary  = lPltfSummary_WP6,
      lRecrPars     = lRecrPars(accrual_param),
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc()
    )
  
  return(lPltfDsgn)
  
}
