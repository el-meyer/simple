% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lAnls.R
\name{lAnls}
\alias{lAnls}
\alias{new_lAnls}
\alias{validate_lAnls}
\title{Run Analysis}
\usage{
new_lAnls(
  fnAnls = function(lPltfTrial, lAddArgs) {
 },
  lAddArgs = list()
)

validate_lAnls(x)

lAnls(
  endpoint = "binary",
  analysis_function_binary = function(x) {
     stats::prop.test(table(x$Arm,
    x$Outcome))$p.value
 },
  analysis_function_continuous = function(x) {
     stats::t.test(Outcome ~ Arm, data =
    x)$p.value
 },
  group1 = c("C", "All"),
  group2
)
}
\description{
Functions and rules for conducting analyses of class lAnls
}
