#' Creates a simple platform trial design of class lPltfDsgn
#' 
#' @param effect_scenario    One of the three effect scenarios specified (1 == all not effective,
#'                           2 == all moderately effective, 3 == Rotation between effects)
#' 
#' @param recr_speed         How many patients per time unit are enrolled? Poisson distribution
#' 
#' @param ISA_add            After how many weeks will a new treatment enter
#' 
#' @param max_time           Number of weeks until which ISAs can be added to platform trial
#'
#' @param futility_stop      Boolean; whether or not the trial can be stopped at interim for futility
#' 
#' @param ISA_start          Number of ISAs when trial is initiated
#' 
#' @param lag_units          Number of time units until outcome is observed
#' 
#' @param alpha              Significance level for one-sided p-value
#'
#'
#' @return Object of class lPltfDsgn to be supplied to fnRunSingleTrialSim or modified later
#'
#' @examples
#' 
#' lPltfDsgn <- 
#'  fnTestDesign(
#'    effect_scenario = 3,
#'    recr_speed = 28,
#'    ISA_add = 2,
#'    max_time = 0,
#'    futility_stop = TRUE,
#'    ISA_start = 3,
#'    lag_units = 1
#'  )
#' 
#' out1 <- fnRunSingleTrialSim(lPltfDsgn, bRetainSnaps = TRUE)
#' ocs1 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 5)
#' ocs2 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 300, nCores = 7)
#' out2 <- fnRunSingleTrialSim(fnTestDesign())
#'
#' @export
fnTestDesign <- function(
  effect_scenario = 1,
  recr_speed = 20/4.25,
  ISA_add = 12,
  max_time = 255,
  futility_stop = TRUE,
  ISA_start = 3,
  lag_units = 6,
  alpha = 0.05
) {
  
  # Create all sort of warning messages and stop messages
  
  
  # add all ISAs separately
  lIntrDsgn <- list()
  
  
  lCheckEnrl_Control <- 
    new_lCheckEnrl(
      
    fnCheckEnrl = function(lPltfTrial, lAddArgs) {
      
      # It is expected that in lAddArgs we will find the current ID under "current_id"
        
        if (all(!sapply(lPltfTrial$isa[-1], function(x) x$bEnrl))) {
          
          if (lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl) {
            # If this is the first time unit in which the enrollment is not active
            print(
              paste0(
                "ISA ",
                lAddArgs$current_id,
                " has stopped enrollment at time ",
                lPltfTrial$lSnap$dCurrTime
              )
            )
            
            lPltfTrial$lSnap$dExitIntr <- lPltfTrial$lSnap$dExitIntr + 1
            lPltfTrial$isa[[lAddArgs$current_id]]$bEnrl <- FALSE
            lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime <- lPltfTrial$lSnap$dCurrTime
            
          }
          
        }
      
      return(lPltfTrial)
      
    },
    lAddArgs   = list()
  )
  
  
  lPatOutcome_bivariate_normal <- function(vMu, mSigma, nLag) {
    
    new_lPatOutcome(
      fnPatOutcome = function(lPltfTrial, lAddArgs) {
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        
        for (i in 1:nrow(lPltfTrial$isa[[lAddArgs$current_id]]$tempPats)) {
              
          # Simulate Outcome
          outcomes <- mvtnorm::rmvnorm(1, mean = vMu, sigma = mSigma)
          
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$BL[i] <- outcomes[1]
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$FU[i] <- outcomes[2]
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$DIFF[i] <- outcomes[2] - outcomes[1]
            
          lPltfTrial$isa[[lAddArgs$current_id]]$tempPats$OutObsTime[i] <- 
            lPltfTrial$lSnap$dCurrTime + lAddArgs$nLag
          
        }
        
        return(lPltfTrial)
        
      },
      
      lAddArgs   = list(vMu = vMu, mSigma = mSigma, nLag = nLag)
      
    )
  }
  
  lAnls_ControlISA <- 
    new_lAnls(
      fnAnls = function(lPltfTrial, lAddArgs) {
        
        # Should never be triggered but keep for plausiblity checks
        
        # Save Analysis Dataset and Timing as well
        lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[lAddArgs$nMstn]] <- 
          list()
        
        print(
          paste0(
            "Is this a mistake? An analysis for ISA ",
            lAddArgs$current_id,
            " was conducted at time ",
            lPltfTrial$lSnap$dCurrTime
          )
        )
        
        return(lPltfTrial)
        
      },
      
      lAddArgs   = list()
    )
  
  
  lSynthRes_ControlISA <- 
    
    new_lSynthRes(
      fnSynthRes = function(lPltfTrial, lAddArgs) {
        
        # If no arm is active anymore, close control arm
        
        if (all(!sapply(lPltfTrial$isa[-1], function(x) is.na(x$nEndTime)))) {
          lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Other"
        }
        
        return(lPltfTrial)
        
      },
      lAddArgs   = list()
    )
  
  
  lAnls_TestDesign <- 
    new_lAnls(
      fnAnls = function(lPltfTrial, lAddArgs) {
        
        # Conduct an ANCOVA using the treatment effect and baseline value as covariates
        # compare against concurrent controls
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        # and that under "nMstn" we will find the number of milestone that was reached
        
        # get patients treated with treatment
        trt_data <- 
          do.call(
            rbind.data.frame, 
            lPltfTrial$isa[[lAddArgs$current_id]]$lPats
          )
        
        trt_data$trt <- 1
        
        # get all control patients
        ctrl_data <- 
          do.call(
            rbind.data.frame, 
            lPltfTrial$isa[[1]]$lPats
          )
        
        ctrl_data$trt <- 0
        
        # filter to only concurrent control data
        
        later <- which(ctrl_data$InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime)
        earlier <- which(ctrl_data$InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime)
        
        ctrl_data_concurrent <- 
          ctrl_data[intersect(later, earlier), ]
        
        # ctrl_data_concurrent <- 
        #   subset(
        #     ctrl_data,
        #     InclusionTime <= lPltfTrial$isa[[lAddArgs$current_id]]$nEndEnrlTime,
        #     InclusionTime >= lPltfTrial$isa[[lAddArgs$current_id]]$nStartTime
        #   )
        
        # get final analysis data
        analysis_data <- 
          rbind(
            trt_data,
            ctrl_data_concurrent
          )
        
        # Factor treatment
        analysis_data$trt <- factor(analysis_data$trt)
        
        # get results from ANCOVA
        results <- 
          lm(
            DIFF ~ BL + trt,
            data = analysis_data
          )
        
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
      
      lAddArgs   = list()
    )
  
  
  lSynthRes_TestDesign <- 
    new_lSynthRes(
      fnSynthRes = function(lPltfTrial, lAddArgs) {
        
        nAnls <- length(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses)
        
        # get one sided p-value
        # für interim analyse: p>0.25 für futility
        # für finale: p-Wert < 0.05 für efficacy
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        if (nAnls > 0 & is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
          
          # get summary of results
          overview <- summary(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results)
          
          one_sided_p <- 
            ifelse(
              overview$coef[3,1] < 0,
              overview$coef[3,4] / 2,
              1 - overview$coef[3,4] / 2
            )
          
          if (nAnls == 1) {
            
            if (lAddArgs$futility) {
            
              if (one_sided_p > 0.25) {
                lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Futility"
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
            
          } else if (nAnls == 2) {
            
            if (one_sided_p < lAddArgs$alpha) {
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
            
          } else {
            stop("It seems to many analyses were conducted")
          }
          
        }
        
        return(lPltfTrial)
        
      },
      lAddArgs   = list(futility = futility_stop, alpha = alpha)
    )
  
  
  # calculate maximum number of interventions
  # stop inclusion of ISAs at week max_time
  # start with 3
  max_intr <- ceiling(1 + ISA_start + max_time/ISA_add)
  
  if (effect_scenario == 1) {
    eff_list <- 
      rep(
        list(list(
          mean = c(32, 20), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2))
        ), 
        max_intr
      )
  } else if (effect_scenario == 2) {
    eff_list <- 
      rep(
        list(list(
          mean = c(32, 16), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2))
        ), 
        max_intr
      )
  } else if (effect_scenario == 3) {
    eff_list <- 
      rep(
        list(
          list(
            mean = c(32, 20), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2)
          ),
          list(
            mean = c(32, 17.5), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2)
          ),
          list(
            mean = c(32, 16), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2)
          ),
          list(
            mean = c(32, 14.3), sigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2)
          )
        ), 
        length.out = max_intr
      )
  } else {
    stop("Invalid Effect Scenario Submitted.")
  }
  
  
  # Control
  lIntrDsgn[[1]] <- 
    
    list(
      
      # need some maximum
      lInitIntr       = 
        lInitIntr(
          cIntrName = "", 
          cArmNames = c("C"), 
          nMaxNIntr = 1500
        ),
      
      lAllocArm       = lAllocArm(),
      
      lPatOutcome     = 
        lPatOutcome_bivariate_normal(
          vMu = c(32, 20), 
          mSigma = matrix(c(31.75, 13.4, 13.4, 123.96), ncol = 2),
          nLag = lag_units
        ),
      
      lCheckAnlsMstn  = lCheckAnlsMstn(bInclude = FALSE),
      

      lAnls           = lAnls_ControlISA,
      

      lSynthRes       = lSynthRes_ControlISA,
      
      
      lCheckEnrl      = lCheckEnrl_Control
    )
  
  # Experimental Treatments
  for (i in 2:max_intr) {
    
    lIntrDsgn[[i]] <- 
      
      list(
        
        lInitIntr       = 
          lInitIntr(
            cIntrName = "", 
            cArmNames = c("T"), 
            nMaxNIntr = 90
          ),
        
        
        lAllocArm       = lAllocArm(),
        
        
        lPatOutcome     = 
          lPatOutcome_bivariate_normal(
            vMu = eff_list[[i-1]]$mean, 
            mSigma = eff_list[[i-1]]$sigma, 
            nLag = lag_units
          ),
        
        
        lCheckAnlsMstn  = 
          lCheckAnlsMstn(
            vInfTimes = c(0.5, 1), 
            column = "OutObsTime"
          ),
        

        lAnls           = lAnls_TestDesign,

        
        lSynthRes       = lSynthRes_TestDesign,
        
        
        lCheckEnrl      = lCheckEnrl()
      )
  }
  
  
  
  lPltfSummary_TestDesign <- 
    new_lPltfSummary(
      
      fnPltfSummary = function(lPltfTrial, lAddArgs) {
        
        # Prepare return list
        ret <- list(
          Decision               = sapply(lPltfTrial$isa, function(x) x$cEndReason),
          Start_Time             = sapply(lPltfTrial$isa, function(x) x$nStartTime),
          N_ISA                  = length(lPltfTrial$isa),
          Total_Time             = lPltfTrial$lSnap$dCurrTime,
          Entry_Time_ISA         = sapply(lPltfTrial$isa, function(x) x$nStartTime),
          Time_ISA               = 
            sapply(lPltfTrial$isa, function(x) x$nEndTime) - 
            sapply(lPltfTrial$isa, function(x) x$nStartTime),
          # This gets all allocated patients (more patients can be allocated than have outcomes observed)
          ISA_N_Alloc            = 
            sapply(
              lPltfTrial$isa, 
              function(x) nrow(do.call(rbind.data.frame, x$lPats))
            ),
          # This gets all patients with observed outcomes
          ISA_N_Obs              = 
            sapply(
              lPltfTrial$isa, 
              function(x) nrow(subset(do.call(rbind.data.frame, x$lPats), !is.na(OutObsTime)))
            ),
          
          Total_N_Alloc          = 
            sum(
              sapply(
                lPltfTrial$isa, 
                function(x) nrow(do.call(rbind.data.frame, x$lPats))
              ), 
              na.rm = TRUE
            ),
          Total_N_Obs            = 
            sum(
              sapply(
                lPltfTrial$isa, 
                function(x) nrow(subset(do.call(rbind.data.frame, x$lPats), !is.na(OutObsTime)))
              ),
              na.rm = TRUE
            ),
          Total_N_Control_Obs    = nrow(do.call(rbind.data.frame, lPltfTrial$isa[[1]]$lPats)),
          IA_Time_ISA            = 
            c(
              NA, 
              sapply(lPltfTrial$isa[-1], function(x) x$lAnalyses[[1]]$analysis_time)
            ),
          N_Conc_Control         = 
            sapply(
              lPltfTrial$isa[-1], 
              function(x) nrow(subset(x$lAnalyses[[length(x$lAnalyses)]]$analysis_data, trt == 0))
            ),
          ISA_IA_Stop            = 
            as.numeric(
              sapply(
                lPltfTrial$isa[-1], 
                function(x) length(x$lAnalyses[[length(x$lAnalyses)]]$analysis_data) == 1
              )
            )
        )
        
        return(ret)
        
      },
      # Pass true response rates to summary function
      lAddArgs   = list()
    )
  
  # to fill later
  lOCSynth_TestDesign <- 
    new_lOCSynth(
      
      fnOCSynth = function(lIndTrials, lAddArgs) {
        
        lOCs <- 
          list(
            
            # use as.matrix in case only one treatment is in the trial to avoid matrix to vector conversion
            
            ISA_Total_Time     = rowMeans(sapply(lIndTrials, function(x) x$Time_ISA)),
            ISA_Entry_Time     = rowMeans(sapply(lIndTrials, function(x) x$Entry_Time_ISA)),
            ISA_IA_Time        = rowMeans(sapply(lIndTrials, function(x) x$IA_Time_ISA)),
            ISA_Avg_IA_Stop    = rowMeans(sapply(lIndTrials, function(x) x$ISA_IA_Stop)),
            ISA_Success        = c(NA, rowMeans(as.matrix(sapply(lIndTrials, function(x) x$Decision[-1]) == "Efficacy"))),
            ISA_N_Obs          = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Obs)),
            ISA_N_Alloc        = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Alloc)),
            
            Avg_N_Trial_Obs    = mean(sapply(lIndTrials, function(x) x$Total_N_Obs)),
            Avg_N_Trial_Alloc  = mean(sapply(lIndTrials, function(x) x$Total_N_Alloc)),
            Avg_Time           = mean(sapply(lIndTrials, function(x) x$Total_Time)),
            Avg_N_Control      = mean(sapply(lIndTrials, function(x) x$Total_N_Control_Obs)),
            Avg_N_Conc_Control = rowMeans(sapply(lIndTrials, function(x) x$N_Conc_Control)),
            Avg_Success        = mean(rowMeans(as.matrix(sapply(lIndTrials, function(x) x$Decision[-1]) == "Efficacy")))
            
          )
        
        return(lOCs)
        
      },
      
      lAddArgs   = list()
      
    )
  
  lRecrPars_TestDesign <- 
    new_lRecrPars(
      fnRecrProc = function(lPltfTrial, lAddArgs) {
        rpois(1, lAddArgs$rate)
      },
      lAddArgs = list(rate = recr_speed)
    )
  
  lNewIntr_TestDesign <- 
    new_lNewIntr(
      fnNewIntr  = function(lPltfTrial, lAddArgs) {
        
        if (lPltfTrial$lSnap$dCurrTime == 1) {
          dAdd <- ISA_start + 1
          
        # if it has been x time units since last inclusion, add one ISA until week max_time
        } else if 
         (lPltfTrial$lSnap$dCurrTime == max(lPltfTrial$lSnap$vIntrInclTimes) + lAddArgs$nTimeDiff &
            lPltfTrial$lSnap$dCurrTime < max_time + 1) {
          dAdd <- 1
        } else {
          dAdd <- 0
        }
        return(dAdd)
      },
      lAddArgs      = list(nTimeDiff = ISA_add)
    )
  
  lUpdIntrAlloc_TestDesign <-
    # Squareroot und >=35% zu Control
    # Assume ISA1 is control
    new_lUpdIntrAlloc(
      fnUpdIntrAlloc = function(lPltfTrial, lAddArgs) {
        # get number of active ISAs
        nActvISA <- sum(sapply(lPltfTrial$isa, function(x) x$bEnrl))
        
        # change something only if nActvISA is > 0
        if (nActvISA > 0) {
          
          # set control weight
          lPltfTrial$lSnap$isa_temp[[1]]$dAlloc <- 
            max(
              (0.35/0.65)*(nActvISA - 1),
              sqrt(nActvISA - 1)
            )
          
          # set weights for all other ISAs
          for (i in 2:length(lPltfTrial$isa)) {
            lPltfTrial$lSnap$isa_temp[[i]]$dAlloc <- 
              ifelse(
                lPltfTrial$isa[[i]]$bEnrl,
                1,
                0
              )
          }
          
        } else {
          
          # otherwise set all dAlloc to 0
          for (i in 1:length(lPltfTrial$isa)) {
            lPltfTrial$lSnap$isa_temp[[i]]$dAlloc <- 0
          }
          
        }
        
        return(lPltfTrial)
        
      },
      lAddArgs   = list()
    )
  
  
  lPltfDsgn <- 
    lPltfDsgn(
      lAddIntr      = lAddIntr(),
      lAddPats      = lAddPats(),
      lAllocIntr    = lAllocIntr(),
      lIntrDsgn     = lIntrDsgn,
      lNewIntr      = lNewIntr_TestDesign,
      lOCSynth      = lOCSynth_TestDesign,
      lPltfSummary  = lPltfSummary_TestDesign,
      lRecrPars     = lRecrPars_TestDesign,
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc_TestDesign
    )
  
  return(lPltfDsgn)
  
}
