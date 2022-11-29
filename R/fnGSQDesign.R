#' Creates a simple platform trial design using a GSQ plan and continuous endpoints of class lPltfDsgn
#' 
#' @param dMuC               Mean in the control group; by default 0
#' 
#' @param dSigC              Standard Deviation in the control group; by default 1
#' 
#' @param vMuT               Vector of means in the treatment groups; if not equal in length to number of
#'                           investigated treatments, vector will be recycled; be default 1
#'                           
#' @param vSigT              Vector of stardard deviations in the treatment groups; if not equal in length
#'                           to number of investigated treatments, vector will be recycled; be default 1
#'                           
#' @param nOutObsLag         Time lag in observing patient outcomes; by default 0
#' 
#' @param nRecrSpeed         How many patients per time unit are enrolled? Default is 10
#' 
#' @param nMaxISA            Maximum number of ISAs to include in platform; Default is 10
#' 
#' @param nMaxNIntr          Maximum sample size in treatment arms; Default is 20
#' 
#' @param vAnalysisTimes     Vector giving timings of interim analyses in information fractions; Default is c(1)
#' 
#' @param nIntrInitial       Number of treatments that start in the platform trial alongside control; Default 1
#' 
#' @param bReplaceIntr       Should outgoing treatments be replaced with new treatments? Default is FALSE
#' 
#' @param dProbReplaceIntr   Probability with which outgoing treatments should be replaced. Default is 1.
#' 
#' @param nWeeksNewIntr      After how many weeks should Treatments be included? 
#'                           If NULL, treatments are not included based on time. Default is NULL. 
#'                           
#' @param dProbWeeksNewIntr  Probability with which treatments are included based on time. Default is 1.
#' 
#' @param cAllocRatio        How are patients allocated to treatments. Options are "balanced" or "squareroot".
#'                           Default is balanced.
#'                           
#' @param vEffBounds         Vector of same length as vAnalysisTimes that gives the efficacy bounds at the
#'                           different analysis time points
#'
#' @param vFutBounds         Vector of same length as vAnalysisTimes that gives the futility bounds at the
#'                           different analysis time points
#'
#'
#' @return Object of class lPltfDsgn to be supplied to fnRunSingleTrialSim or modified later
#'
#' @examples
#' 
#' lPltfDsgn <- 
#'  fnGSQDesign( 
#'   dMuC              = 0,
#'   dSigC             = 1,
#'   vMuT              = 0.25,
#'   vSigT             = 1,
#'   nOutObsLag        = 2,
#'   nRecrSpeed        = 5,
#'   nMaxISA           = 4,
#'   nMaxNIntr         = 50,
#'   vAnalysisTimes    = c(0.5, 1),
#'   nIntrInitial      = 1,
#'   bReplaceIntr      = TRUE,
#'   dProbReplaceIntr  = 0.5,
#'   nWeeksNewIntr     = 2,
#'   dProbWeeksNewIntr = 0.5,
#'   cAllocRatio       = "balanced",
#'   vEffBounds        = c(0.01, 0.04),
#'   vFutBounds        = c(0.25, 0.05)
#'  )
#' 
#' out1 <- fnRunSingleTrialSim(lPltfDsgn, bRetainSnaps = TRUE)
#' ocs1 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 5)
#' ocs2 <- fnSimDsgnOC(lPltfDsgn = lPltfDsgn, nIter = 300, nCores = 7)
#' out2 <- fnRunSingleTrialSim(fnGSQDesign())
#'
#' @export
fnGSQDesign <- function(
  dMuC              = 0,
  dSigC             = 1,
  vMuT              = 1,
  vSigT             = 1,
  nOutObsLag        = 0,
  nRecrSpeed        = 10,
  nMaxISA           = 10,
  nMaxNIntr         = 20,
  vAnalysisTimes    = c(1),
  nIntrInitial      = 1,
  bReplaceIntr      = FALSE,
  dProbReplaceIntr  = 1,
  nWeeksNewIntr     = NA,
  dProbWeeksNewIntr = 1,
  cAllocRatio       = "balanced",
  vEffBounds        = c(0.05),
  vFutBounds        = c(0.05)
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
  
  
  lSynthRes_GSQDesign <- 
    new_lSynthRes(
      fnSynthRes = function(lPltfTrial, lAddArgs) {
        
        nAnls <- length(lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses)
        
       # Result from Analysis function is two-sided p-value
        
        # It is expected that in lAddArgs we will find the current ID under "current_id"
        if (nAnls > 0 & is.na(lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason)) {
          
          pval <- lPltfTrial$isa[[lAddArgs$current_id]]$lAnalyses[[nAnls]]$results
          
          fut_bound <- vFutBounds[nAnls]
          eff_bound <- vEffBounds[nAnls]
          
          if (pval > fut_bound) {
            
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
            
          } else if (pval < eff_bound) {
            
            lPltfTrial$isa[[lAddArgs$current_id]]$cEndReason <- "Efficacy"
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
      lAddArgs   = list(vEffBounds = vEffBounds, vFutBounds = vFutBounds)
    )
  
  
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
        lPatOutcome(
          cGroups = c("C"),
          dTheta  = dMuC,
          dSigma  = dSigC,
          dTrend  = 0,
          nLag    = nOutObsLag
        ),
      
      lCheckAnlsMstn  = lCheckAnlsMstn(bInclude = FALSE),
      
      
      lAnls           = lAnls_ControlISA,
      
      
      lSynthRes       = lSynthRes_ControlISA,
      
      
      lCheckEnrl      = lCheckEnrl_Control
    )
  
  
  # create vector of means and standard deviations to draw from for ISA list
  
  vMusT <- 
    rep_len(
      vMuT,
      length.out = nMaxISA
    )
  
  vSigsT <- 
    rep_len(
      vSigT,
      length.out = nMaxISA
    )
  
  
  # Experimental Treatments
  for (i in 2:(nMaxISA + 1)) {
    
    lIntrDsgn[[i]] <- 
      
      list(
        
        lInitIntr       = 
          lInitIntr(
            cIntrName = "", 
            cArmNames = c("T"), 
            nMaxNIntr = nMaxNIntr
          ),
        
        
        lAllocArm       = lAllocArm(),
        
        
        lPatOutcome     = 
          lPatOutcome(
            cGroups = c("T"),
            dTheta  = vMusT[i-1],
            dSigma  = vSigsT[i-1],
            dTrend  = 0,
            nLag    = nOutObsLag
          ),
        
        
        lCheckAnlsMstn  = 
          lCheckAnlsMstn(
            vInfTimes = vAnalysisTimes, 
            column    = "OutObsTime"
          ),
        
        
        lAnls           =
          lAnls(
            endpoint = "continuous",
            analysis_function_continuous = function(x) {
              stats::t.test(
                Outcome ~ Arm,
                data = x
              )$p.value
            },
            group1 = c("C", "Conc"), 
            group2 = c("T", "Intr")
          ),
        
        
        lSynthRes       = lSynthRes_GSQDesign,
        
        
        lCheckEnrl      = lCheckEnrl()
      )
  }
  
  
  
  lPltfSummary_GSQDesign <- 
    new_lPltfSummary(
      
      fnPltfSummary = function(lPltfTrial, lAddArgs) {
        
        # calculate success per treatment effect -> save as matrix and then in synthOC only use second line
        
        unique_effects <- unique(lAddArgs$vMuT)
        
        mResTrtEff <- matrix(nrow = 2, ncol = length(unique_effects))
        
        # for all treatment effects
        for (i in 1:length(unique_effects)) {
          
          eff <- unique_effects[i]
          
          # check which ISAs were assigned this treatment effect
          indices <- which(lAddArgs$vMusT == eff) + 1
          # Remove ISAs that were never realised
          indices <- indices[indices < length(lPltfTrial$isa)]
          
          mResTrtEff[1, i] <- eff
          mResTrtEff[2, i] <- mean(sapply(lPltfTrial$isa[indices], function(x) x$cEndReason == "Efficacy"))
          
        }
        
        # 
        # calculate success probabilites per ISA -> different number of ISAs, so blow up the ones that 
        # had less ISAs than the one with the most ISAs
        # 
        # calculate "at risk" for above success probabilities which can be used to derive standard errors
        # 
        # later we might add something that crosses success probabilities per ISAs and per treatment effect,
        # if e.g. treatment effects are random
        
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
          N_Conc_Control         = 
            sapply(
              lPltfTrial$isa[-1], 
              function(x) nrow(subset(x$lAnalyses[[length(x$lAnalyses)]]$analysis_data, Arm == "C"))
            ),
          Success_per_TE         = mResTrtEff
        )
        
        return(ret)
        
      },
      # Pass true response rates to summary function
      lAddArgs   = list(vMuT = vMuT, vMusT = vMusT)
    )
  
  # to fill later
  lOCSynth_GSQDesign <- 
    new_lOCSynth(
      
      fnOCSynth = function(lIndTrials, lAddArgs) {
        
        lOCs <- 
          list(
            
            # use as.matrix in case only one treatment is in the trial to avoid matrix to vector conversion
            
            ISA_Total_Time     = rowMeans(sapply(lIndTrials, function(x) x$Time_ISA)),
            ISA_Entry_Time     = rowMeans(sapply(lIndTrials, function(x) x$Entry_Time_ISA)),
            ISA_Success        = c(NA, rowMeans(as.matrix(sapply(lIndTrials, function(x) x$Decision[-1]) == "Efficacy"))),
            ISA_N_Obs          = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Obs)),
            ISA_N_Alloc        = rowMeans(sapply(lIndTrials, function(x) x$ISA_N_Alloc)),
            
            Avg_N_Trial_Obs    = mean(sapply(lIndTrials, function(x) x$Total_N_Obs)),
            Avg_N_Trial_Alloc  = mean(sapply(lIndTrials, function(x) x$Total_N_Alloc)),
            Avg_Time           = mean(sapply(lIndTrials, function(x) x$Total_Time)),
            Avg_N_Control      = mean(sapply(lIndTrials, function(x) x$Total_N_Control_Obs)),
            Avg_Success        = mean(rowMeans(as.matrix(sapply(lIndTrials, function(x) x$Decision[-1]) == "Efficacy"))),
            Avg_N_Conc_Control = rowMeans(sapply(lIndTrials, function(x) x$N_Conc_Control))
            
          )
        
        return(lOCs)
        
      },
      
      lAddArgs   = list()
      
    )
  
  
  lNewIntr_GSQDesign <- 
    new_lNewIntr(
      fnNewIntr  = function(lPltfTrial, lAddArgs) {
        
        if (lPltfTrial$lSnap$dCurrTime == 1) {
          dAdd <- nIntrInitial + 1
        } else {
          
          # Initialize
          dAdd <- 0
          
          # check if maximum has been reached
          if (length(lPltfTrial$lSnap$vIntrInclTimes) < lAddArgs$nMaxIntr) {
            
            # check if a treatment should be added based on time
            if (!is.na(lAddArgs$nTimeDiff)) {
              # Check if enough weeks have passed
              if (lPltfTrial$lSnap$dCurrTime == max(lPltfTrial$lSnap$vIntrInclTimes) + lAddArgs$nTimeDiff) {
                # add with probability
                dAdd <- dAdd + sample(c(0, 1), 1, prob = c(1 - lAddArgs$pW, lAddArgs$pW))
              }
            }
            
            # check if there are any outgoing treatments
            if (lAddArgs$replace) {
              dAdd <- 
                dAdd + 
                lPltfTrial$lSnap$dExitIntr * sample(c(0, 1), 1, prob = c(1 - lAddArgs$pR, lAddArgs$pR))
            }
            
          }
          
        }
        
        return(dAdd)
      },
      lAddArgs      = 
        list(
          nMaxIntr  = nMaxNIntr, 
          nTimeDiff = nWeeksNewIntr,
          pW        = dProbWeeksNewIntr,
          replace   = bReplaceIntr,
          pR        = dProbReplaceIntr
        )
    )
  
  lUpdIntrAlloc_GSQDesign <-
    # Squareroot und >=35% zu Control
    # Assume ISA1 is control
    new_lUpdIntrAlloc(
      fnUpdIntrAlloc = function(lPltfTrial, lAddArgs) {
        
        # get number of active ISAs
        nActvISA <- sum(sapply(lPltfTrial$isa, function(x) x$bEnrl))
        
        
        # set weights for all other ISAs
        for (i in 2:length(lPltfTrial$isa)) {
          lPltfTrial$lSnap$isa_temp[[i]]$dAlloc <- 
            ifelse(
              lPltfTrial$isa[[i]]$bEnrl,
              1,
              0
            )
        }
        
        # check if balanced or square root
        
        if (lAddArgs$type == "balanced") {
          
          # set control weight
          lPltfTrial$lSnap$isa_temp[[1]]$dAlloc <- 1
          
        } else if (lAddArgs$type == "squareroot") {
          
          # set control weight
          lPltfTrial$lSnap$isa_temp[[1]]$dAlloc <- 
            max(
              (0.35/0.65)*(nActvISA - 1),
              sqrt(nActvISA - 1)
            )
          
        }
       
        return(lPltfTrial)
        
      },
      lAddArgs   = list(type = cAllocRatio)
    )
  
  
  lPltfDsgn <- 
    lPltfDsgn(
      lAddIntr      = lAddIntr(),
      lAddPats      = lAddPats(),
      lAllocIntr    = lAllocIntr(),
      lIntrDsgn     = lIntrDsgn,
      lNewIntr      = lNewIntr_GSQDesign,
      lOCSynth      = lOCSynth_GSQDesign,
      lPltfSummary  = lPltfSummary_GSQDesign,
      lRecrPars     = lRecrPars(nRecrSpeed),
      lSimBase      = lSimBase(),
      lSnap         = lSnap(),
      lStopRule     = lStopRule(bNoActive = TRUE),
      lUpdIntrAlloc = lUpdIntrAlloc_GSQDesign
    )
  
  return(lPltfDsgn)
  
}

