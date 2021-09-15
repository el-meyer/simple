
# Tests to see whether lRecrPars works as intended

# Initialize lRecrPars and plot standard enrollment
x <- lRecrPars(lambda = 4)
plot(x)
summary(x)

x <- lRecrPars(lambda = 10)
plot(x)
summary(x)


# Enrollment where every 7th time unit more patients enter
x <- 
  new_lRecrPars(
    fnRecrProc = function(lGlobVars, lAddArgs) {
      if(lGlobVars$lVars$dCurrTime %% 7 == 0) {
        rpois(1, lambda = lAddArgs$lambda2)
      } else {
        rpois(1, lambda = lAddArgs$lambda1)
        }
      },
    lAddArgs   = list(lambda1 = 2, lambda2 = 20)
  )
plot(x)
summary(x)


# exponential enrollment that is capped
x <- 
  new_lRecrPars(
    fnRecrProc = function(lGlobVars, lAddArgs) {
        min(lAddArgs$growth ^ lGlobVars$lVars$dCurrTime, lAddArgs$cap)
    },
    lAddArgs   = list(growth = 1.5, cap = 80)
  )

plot(
  x, 
  dCurrTime = 1:15, 
  dActvIntr = rep(1, 15)
)
summary(x)


# more complex 
# poisson, but with increase per active arm

x <- 
  new_lRecrPars(
    fnRecrProc = function(lGlobVars, lAddArgs) {
      rpois(1, lambda = lAddArgs$lambda) * lGlobVars$lVars$dActvIntr * lAddArgs$mult
    },
    lAddArgs   = list(lambda = 3, mult = 6)
  )

plot(
  x, 
  dActvIntr = c(
    rep(1, 15), 
    rep(2, 10), 
    rep(3, 10), 
    rep(2, 10), 
    rep(1,7)
  )
)
summary(x)


# Like before, but now recruitment depends on other global variable that enables recruitment only during a certain time window

x <- 
  new_lRecrPars(
    fnRecrProc = function(lGlobVars, lAddArgs) {
      pat <- rpois(1, lambda = lAddArgs$lambda) * lGlobVars$lVars$dActvIntr * 6
      ifelse(lGlobVars$lVars$bEnrOpen, pat, 0)
    },
    lAddArgs   = list(lambda = 4)
  )

plot(
  x, 
  dActvIntr = c(
    rep(1, 15), 
    rep(2, 10), 
    rep(3, 10), 
    rep(2, 10), 
    rep(1,7)
  ),
  bEnrOpen = c(
    rep(FALSE, 10),
    rep(TRUE, 30),
    rep(FALSE, 12)
  )
)
summary(x)
