% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lNewIntr.R
\name{lNewIntr}
\alias{lNewIntr}
\alias{new_lNewIntr}
\alias{validate_lNewIntr}
\alias{plot.lNewIntr}
\alias{summary.lNewIntr}
\title{ISA inclusion rules}
\usage{
new_lNewIntr(
  fnNewIntr = function(lPltfTrial, lAddArgs) {
 },
  lAddArgs = list()
)

validate_lNewIntr(x)

lNewIntr(nMaxIntr, nStartIntr, vArrTimes = NULL)

\method{plot}{lNewIntr}(
  x,
  dCurrTime = 1:52,
  cIntrTime = "fixed",
  dIntrTimeParam = 10,
  nIntrStart = 1,
  ...
)

\method{summary}{lNewIntr}(x, ...)
}
\arguments{
\item{fnNewIntr}{Function that checks how many new ISAs should be added to the platform}

\item{lAddArgs}{Further arguments used in fnNewIntr}
}
\description{
Rules for ISA starting times of class lNewIntr
}
\examples{

x <- lNewIntr(4,4)
validate_lNewIntr(x)
plot(x)
summary(x)

}
