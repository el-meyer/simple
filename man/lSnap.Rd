% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lSnap.R
\name{lSnap}
\alias{lSnap}
\alias{new_lSnap}
\alias{validate_lSnap}
\alias{summary.lSnap}
\title{List of current trial snapshot}
\usage{
new_lSnap(
  fnSnap = function(lPltfTrial, lAddArgs) {
 },
  lAddArgs = list()
)

validate_lSnap(x)

lSnap()

\method{summary}{lSnap}(x, ...)
}
\arguments{
\item{lAddArgs}{Further arguments used in fnAddIntr}

\item{fnAddIntr}{Function which will add ISAs}
}
\description{
List of snapshot variables to be extracted in simulations and passed to different methods
}
