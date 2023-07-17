
# Initialising platform design object
lPltfDsgn <- 
  fnWP6simple()
# Simulate trial and store results in "out"
out <- 
  fnRunSingleTrialSim(lPltfDsgn)

# Simulating trial "nIter" times and summarizing results
ocs <- 
  fnSimDsgnOC(
    lPltfDsgn = lPltfDsgn, 
    nIter = 10000, 
    nCores = 5
  )

# By default, final cohort sample size is 150. 
# Change to 200.
sample_size <- 200
# By default, analysis time points are after 50%, 75% 
# and 100% of outcomes are observed. 
# Change to 30%, 60% and 100%
timings <- c(0.3, 0.6, 1)
# Creating platform trial design object
lPltfDsgn <- 
  fnWP6simple(
    n_fin = sample_size,
    analysis_times = timings
  )

# In lPltfTrial$lSnap$dCurrTime, we can always access 
# the current time of the platform trial
custom_accrual_module <- 
  # here we use the helper function to create the module
  new_lRecrPars(
    # The function that calculates the patients entering 
    # per time unit must have these two input parameters
    fnRecrProc = function(lPltfTrial, lAddArgs) {
      # 
      if(lPltfTrial$lSnap$dCurrTime %% 7 == 0) {
        10
      } else {
        2
      }
    },
    lAddArgs   = list()
  )
# Add new module to existing design
lPltfDsgn$lRecrPars <- custom_accrual_module


# Create base platform design
# For the sake of this example increase sample size
# and investigate only one treatment arm
# with a very fast recruitment rate.
# For more information see ?fnSimpleDesign
lPltfDsgn <- 
  fnSimpleDesign(
    sample_size    = 50000,
    intr_at_start  = 1,
    max_intr       = 1,
    recr_speed     = 10000
  )

# Add baseline covariate simulation for patients
# Add age (uniformly distributed between 50 and 80) 
# and gender (70% males [0] and 30% females [1])
custom_baseline_covariates <- 
  # use the default function to create the module
  # requires list of the logic and a vector of names
  lSimBase(
    vars = list(
      list("age", "runif(n, 50, 70)"),
      list("Sex", "sample(0:1, n, T, c(0.55, 0.45))")
    ),
    names = c("Age", "Sex")
  )
# Add new module to existing design
lPltfDsgn$lSimBase <- custom_baseline_covariates

# Add patients response probability based on 
# their simulated covariates
# firstly we need to create a function that converts
# logits to probabilites:
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
# now we can use the helper function to create
# the patient outcome simulation module
custom_outcome <- 
  new_lPatOutcome(
    fnPatOutcome = function(lPltfTrial, lAddArgs) {
      # It is expected that in lAddArgs we will find 
      # the current ID under "current_id". The newly
      # created patients are in the following dataset:
      newpats <- 
        lPltfTrial$isa[[lAddArgs$current_id]]$tempPats
      # For every new patient assign their outcome
      for (i in 1:nrow(newpats)) {
        # If patient was not assigned to an arm, 
        # the outcome is NA.
        if (is.na(newpats$Arm[i])) {
          newpats$Outcome[i] <- NA
          newpats$OutObsTime[i] <- NA
        } else {
          # Get success probability
          prob <- logit2prob(
            -2 +
            newpats$Sex[i] * log(2) +
            newpats$Age[i] * log(0.99) + 
            (newpats$Arm[i] == "T") * log(1.4)
          )
          # Simulate Outcome
          newpats$Outcome[i] <- 
            rbinom(1, 1, prob)
        }
        # Set Outcome Observation Time
        newpats$OutObsTime[i] <- 
          lPltfTrial$lSnap$dCurrTime
      }
      # copy back
      lPltfTrial$isa[[lAddArgs$current_id]]$tempPats <- 
        newpats
      return(lPltfTrial)
    },
    lAddArgs   = list()
  )
# Add new module to existing design
# Note that lPatOutcome is an ISA-level module and hence needs 
# to be modified in lPltfDsgn$lIntrDsgn
lPltfDsgn$lIntrDsgn[[1]]$lPatOutcome <- custom_outcome

# Simulate trial and store results in "out"
out <- 
  fnRunSingleTrialSim(lPltfDsgn)
# Grab dataset for final analysis of treatment 1
df <- 
  out$lFinalSnap$isa[[1]]$lAnalyses[[1]]$analysis_data
# Tabulate relationship between covariates/outcome
library(dplyr)
df %>% 
  mutate(Age_Cat = cut(Age, breaks = 2)) %>% 
  group_by(Sex, Arm, Age_Cat) %>% 
  summarize(SuccessProb = mean(Outcome))
# # A tibble: 8 × 4
# # Groups:   Sex, Arm [4]
# Sex Arm   Age_Cat SuccessProb
# <int> <fct> <fct>         <dbl>
#   1     0 C     (50,60]      0.0732
# 2     0 C     (60,70]      0.0657
# 3     0 T     (50,60]      0.0959
# 4     0 T     (60,70]      0.0927
# 5     1 C     (50,60]      0.140 
# 6     1 C     (60,70]      0.118 
# 7     1 T     (50,60]      0.181 
# 8     1 T     (60,70]      0.161 


# Set analysis module to use logistic regression. 
# This assumes that x is the dataset we are running an analysis on.
# Since we set up the patient simulation appropriately, we can 
# expect the columns Sex, Age, Arm and Outcome to be present int this dataset.
# When running a logistic regression, the p-value for the first covariate 
# (in this case Arm) can be found in coef(summary())[2,4].
f1 <- function(x) {
  coef(
    summary(
      glm(
        Outcome ~ Arm + Sex + Age, 
        data = df, 
        family = "binomial"
      )
    )
  )[2,4]
}

# The analysis module can be initialized using the function lAnls. 
# Input arguments are type of endpoint, analysis funcion to be used and
# how the dataset is constructed, i.e. are we using all control data, 
# only concurrent control data or no data sharing between ISAs.
custom_analysis_module <-
  lAnls(
    endpoint = "binary",
    group1   = c("C", "Conc"),
    group2   = c("T", "Intr"),
    analysis_function_binary = f1,
  )

# Add new module to existing design
# Note that lPatOutcome is an ISA-level module and hence needs 
# to be modified in lPltfDsgn$lIntrDsgn
lPltfDsgn$lIntrDsgn[[1]]$lAnls <- custom_analysis_module

# Simulate trial and store results in "out"
out <- 
  fnRunSingleTrialSim(lPltfDsgn)
