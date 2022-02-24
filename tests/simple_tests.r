
lIntrDsgn <- 
  list(
    list(
      lInitIntr       = lInitIntr(cIntrName = "A", cArmNames = c("C", "T1")),
      lAllocArm       = lAllocArm(),
      lPatOutcome     = lPatOutcome(cGroups = c("C", "T1"), dRates = c(0.1, 0.2), dTrend = 0, nLag = 10),
      lCheckAnlsMstn  = lCheckAnlsMstn(),
      lAnls           = lAnls(group1 = c("C", "Intr"), group2 = c("T1", "Intr")),
      lSynthRes       = lSynthRes(),
      lCheckEnrl      = lCheckEnrl()
    ),
    list(
      lInitIntr       = lInitIntr(cIntrName = "B", cArmNames = c("C", "T2")),
      lAllocArm       = lAllocArm(),
      lPatOutcome     = lPatOutcome(cGroups = c("C", "T2"), dRates = c(0.1, 0.2), dTrend = 0, nLag = 10),
      lCheckAnlsMstn  = lCheckAnlsMstn(),
      lAnls           = lAnls(group1 = c("C", "All"), group2 = c("T2", "Intr")),
      lSynthRes       = lSynthRes(),
      lCheckEnrl      = lCheckEnrl()
    ),
    list(
      lInitIntr       = lInitIntr(cIntrName = "C", cArmNames = c("C", "T3")),
      lAllocArm       = lAllocArm(),
      lPatOutcome     = lPatOutcome(cGroups = c("C", "T3"), dRates = c(0.1, 0.2), dTrend = 0, nLag = 10),
      lCheckAnlsMstn  = lCheckAnlsMstn(),
      lAnls           = lAnls(group1 = c("C", "All"), group2 = c("T3", "Intr")),
      lSynthRes       = lSynthRes(),
      lCheckEnrl      = lCheckEnrl()
    ),
    list(
      lInitIntr       = lInitIntr(cIntrName = "D", cArmNames = c("C", "T4")),
      lAllocArm       = lAllocArm(),
      lPatOutcome     = lPatOutcome(cGroups = c("C", "T4"), dRates = c(0.1, 0.2), dTrend = 0, nLag = 10),
      lCheckAnlsMstn  = lCheckAnlsMstn(),
      lAnls           = lAnls(group1 = c("C", "All"), group2 = c("T4", "Intr")),
      lSynthRes       = lSynthRes(),
      lCheckEnrl      = lCheckEnrl()
    ),
    list(
      lInitIntr       = lInitIntr(cIntrName = "E", cArmNames = c("C", "T5")),
      lAllocArm       = lAllocArm(),
      lPatOutcome     = lPatOutcome(cGroups = c("C", "T5"), dRates = c(0.1, 0.2), dTrend = 0, nLag = 10),
      lCheckAnlsMstn  = lCheckAnlsMstn(),
      lAnls           = lAnls(group1 = c("C", "All"), group2 = c("T5", "Intr")),
      lSynthRes       = lSynthRes(),
      lCheckEnrl      = lCheckEnrl()
    )
  )

lPltfDsgn <- 
  lPltfDsgn(
    lAddIntr      = lAddIntr(2),
    lAddPats      = lAddPats(),
    lAllocIntr    = lAllocIntr(),
    lIntrDsgn     = lIntrDsgn,
    lNewIntr      = lNewIntr(5),
    lPltfSummary  = lPltfSummary(),
    lRecrPars     = lRecrPars(5),
    lSimBase      = lSimBase(),
    lSnap         = lSnap(),
    lStopRule     = lStopRule(bNoActive = TRUE),
    lUpdIntrAlloc = lUpdIntrAlloc()
  )


# Full Run

res <- runSingleTrialSim(lPltfDsgn)



# Check if lPltfTrial is correctly initialized and closed


assign(
  "lPltfTrial",
  do.call(
    match.fun(lPltfDsgn$lFnDef$fnInitialize),
    args = list(
      lPltfDsgn
    )
  )
)

lPltfTrial$lSnap$dExitIntr <- 1

assign(
  "lPltfTrial",
  do.call(
    match.fun(lPltfDsgn$lFnDef$fnAddNewIntr),
    args = list(
      lPltfDsgn  = lPltfDsgn,
      lPltfTrial = lPltfTrial
    )
  )
)

bTrialClose <- 
  do.call(
    match.fun(lPltfDsgn$lStopRule$fnStopRule), 
    args = list(
      lPltfTrial   = lPltfTrial,
      lAddArgs     = lPltfDsgn$lStopRule$lAddArgs
    )
  )

for (i in 1:length(lPltfTrial$isa)) {
  lPltfTrial$isa[[i]]$cEndReason <- "Eff"
}

bTrialClose <- 
  do.call(
    match.fun(lPltfDsgn$lStopRule$fnStopRule), 
    args = list(
      lPltfTrial   = lPltfTrial,
      lAddArgs     = lPltfDsgn$lStopRule$lAddArgs
    )
  )


# Simulate Patients

assign(
  "lPltfTrial",
  do.call(
    match.fun(lPltfDsgn$lFnDef$fnUpdateAlloc),
    args = list(
      lPltfDsgn  = lPltfDsgn,
      lPltfTrial = lPltfTrial
    )
  )
)

assign(
  "lPltfTrial",
  do.call(
    match.fun(lPltfDsgn$lFnDef$fnGenNewPat),
    args = list(
      lPltfDsgn  = lPltfDsgn,
      lPltfTrial = lPltfTrial
    )
  )
)
