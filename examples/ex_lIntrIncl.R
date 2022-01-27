w
# Tests to see whether lIntrIncl works as intended

# Initialize lIntrIncl and plot
x <- lIntrIncl(4)
# in default version, replace every outgoing ISA until a maximum number of ISAs (in this case 4)
validate_lIntrIncl(x)
plot(x)
# default plot assumes an in-trial time for ISAs of 10 time steps
summary(x)

x <- lIntrIncl(2)
plot(x)


# Dont replace outgoing but add an arm every 4 steps
x <- 
  new_lIntrIncl(
    fnIntrIncl  = function(lPltfTrial, lAddArgs) {
      # if it has been 4 time units since last inclusion, add one ISA
      if (lPltfTrial$lSnap$dCurrTime == max(lPltfTrial$lSnap$vIntrInclTimes) + lAddArgs$dTimeDiff) {
        dAdd <- 1
      } else {
        dAdd <- 0
      }
      return(dAdd)
    },
    lAddArgs      = list(dTimeDiff = 4)
  )
plot(x)
summary(x)


# Add ISAs with random probability but never more than two at the same time and never allow more than 5 to run in parallel

x <- 
  new_lIntrIncl(
    fnIntrIncl  = function(lPltfTrial, lAddArgs) {
      # if it has been 4 time units since last inclusion, add one ISA
      if (lPltfTrial$lSnap$dActvIntr < lAddArgs$dIntrMax) {
        dAdd <- min(rbinom(1, 3, 0.4), lAddArgs$dIntrMax - lPltfTrial$lSnap$dActvIntr)
      } else {
        dAdd <- 0
      }
      return(min(2, dAdd))
    },
    lAddArgs      = list(dIntrMax = 5)
  )
plot(x)
summary(x)


# As before, but have period in between of absolute ISA inclusion stop

x <- 
  new_lIntrIncl(
    fnIntrIncl  = function(lPltfTrial, lAddArgs) {
      # if it has been 4 time units since last inclusion, add one ISA
      if (lPltfTrial$lSnap$dActvIntr < lAddArgs$dIntrMax & lPltfTrial$lSnap$bInclAllowed) {
        dAdd <- min(rbinom(1, 3, 0.4), lAddArgs$dIntrMax - lPltfTrial$lSnap$dActvIntr)
      } else {
        dAdd <- 0
      }
      return(min(2, dAdd))
    },
    lAddArgs      = list(dIntrMax = 5)
  )
plot(
  x,
  bInclAllowed = c(rep(TRUE, 15), rep(FALSE, 15), rep(TRUE, 22))
)
summary(x)

