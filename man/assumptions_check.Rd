% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/runmodel.R
\name{assumptions_check}
\alias{assumptions_check}
\title{Multiple Regression Assumption Checking}
\usage{
assumptions_check(model)
}
\arguments{
\item{model}{A \code{lm} model object.  \code{run_model} automatically calls
this function for the model with all blocks of predictors included.}
}
\description{
Multiple Regression Assumption Checking
}
\details{
Creates objects related to multiple regression assumption checking.
  These objects are used by \code{model_output} to produce readable output.
}
\examples{
freeny_model_formulas <- create_formula_objects("y", c("lag.quarterly.revenue")
, c("price.index"))
freeny_models <- create_model_objects(freeny_model_formulas,
dataset = freeny)
freeny_model <- freeny_models[[length(freeny_models)]]
assumptions_check(freeny_model)
}

