#' Automated Multiple Regression Modelling
#'
#' @param outcome The dependent variable of the hierarchical model
#' @param block1 A character vector, with names of variables. The first block of independent variables.
#' @param ...  A character vector, with names of variables. Subsequent blocks of independent variables.
#' @param dataset A data frame containing variables refered to in \code{formulas}, passed to data argument of \code{lm}
#' @param transform.outcome A boolean. If TRUE, a variable transformation of the outcome is substituted in the final model if outcome is non-normal.
#' @details Calls other functions to generate model objects and test them, given specified model parameters and other options.  Formatted output is produced via \code{model_output}
#' @examples
#' runmodel("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))
#'
run_model <- function(outcome, block1, ..., dataset, transform.outcome=F){
  # Main function that calls the others to generate model objects, and test and summarize those model objects
  forms <- create_formula_objects(outcome, block1, ...)
  models <- create_model_objects(forms, dataset)
  top_model <- models[[length(models)]]
  assumptions_check(top_model)
  model_output(models)
}

#' Hierarchical Formula Generation
#'
#' @param outcome The dependent variable of the hierarchical model
#' @param block1 A character vector, with names of variables. The first block of independent variables.
#' @param ...  A character vector, with names of variables. Subsequent blocks of independent variables.
#' @return A list of \code(lm) formulas
#' @examples
#' create_formula_objects("y", c("lag.quarterly.revenue"))
#' create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))
#'
create_formula_objects <- function(outcome, block1, ...){
  # Creates formulas for hierarchical models from blocks of predictors
  # Pass character vectors with names of independent variables corresponding to each block
  blocks <- list(...)
  formula <- as.formula(paste(outcome, "~", paste(block1, collapse="+")))
  formulas <- list()
  if(length(blocks) != 0){
    formulas <- list(formula)
  for(block in blocks){
  formulas[[length(formulas) + 1]] <-  as.formula(paste(formulas[length(formulas)], "+", paste(block, collapse="+")))
  }
  }
  if(length(formulas) == 0){
    formula
  } else {
    formulas
  }
}

#' Hierarchical Regression Model Generation
#'
#' @param formulas A set of \code{lm} formulas, created with create_formula_objects
#' @param dataset A data frame containing variables refered to in \code{formulas}, passed to data argument of \code{lm}
#' @return A list of \code{lm} model objects
#' @examples
#' create_model_objects(create_formula_objects("y", c("lag.quarterly.revenue")), dataset = freeny)
#' create_model_objects(freeny_model_formulas, dataset = freeny)
#'
create_model_objects <- function(formulas, dataset){
  # Creates all of the hierarchical models from a set of formulas created with create_formula_objects
  models <- lapply(X = forms, data=freeny, lm)
}

#' Multiple Regression Assumption Checking
#'
#' @param model A \code{lm} model object.  \code{run_model} automatically calls this function for the model with all blocks of predictors included.
#' @details Creates objects related to multiple regression assumption checking.  These objects are used by \code{model_output} to produce readable output.
#' @examples
#' assumptions_check(freeny_model)
#'
assumptions_check <- function(model){
  # Creates objects needed for assumption checking and output printing
  dw <<- dwtest(model)$statistic
  dwp <<- dwtest(model)$p.value
  #partplots <- avPlots(model)
  residplot <<- plot(predict(model), studres(model), main="Residuals by Predicted value", xlab="Unstandardized Predicted Values", ylab="Studentized Residuals")
  cormat <<- cor(data.frame(lapply(model.frame(model), as.numeric)), use="pairwise.complete.obs")
  vifs <<- vif(model)
  standresids <<- stdres(model)
  cdists <<- cooks.distance(model)
  levplot <<- leveragePlots(model)
  residplot <<- hist(standresids, prob=T, breaks = 30, main = "Plot of Std Residuals", xlab="Std Residuals")
  normresids <<- shapiro.test(standresids)$p.value
  probDist <<- pnorm(stdres(model))
}

#' Multiple Regression Assumption Checking
#'
#' @param models A list of \code{lm} model objects.  A set of model objects created by \code{create_model_object}.
#' @details Creates plots and text output to summarize models and check assumptions via objects created by \code{assumptions_check}.  Uses full model with all predictors.
#' @examples
#' model_check(freeny_models)
#'
model_output <- function(models){
  # Produces plots and prints relevant messages and outputs.
  model <- models[[length(models)]]
  cat("Durbin-Watson = ", dw, "p value = ", dwp, "\n")
  cat("Partial Regression plots (all relationships should be linear):\n")
  residplot
  cat("Correlation Matrix for model (correlation >.70 indicates severe multicollinearity)\n")
  print(cormat)
  cat("Variance inflation factor (<10 desired):\n")
  print(vifs)
  cat("Standardized Residuals (observations > 3.00 problematic):\n")
  res <- sort(abs(standresids), decreasing = T)
  print(res)
  cat("Cook's distance (values >.2 problematic):\n")
  print(sort(cdists, decreasing = T))
  levplot
  residplot
  mean(stdres(model))
  sd(stdres(model))
  curve(dnorm(x,mean(stdres(model)), sd(stdres(model))), col="darkblue", lwd=2, add=TRUE, yaxt="n")
  cat("Normality of standardized model residuals:", " Shapiro-Wilk (p-value): ", normresids, "\n")
  plot(ppoints(length(stdres(model))), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
  abline(0,1)
  cat("Model change statistics\n")
  # Compares the models between each block of predictors
  for(i in 1:(length(models) - 1)){
    modelCompare(models[[i]], models[[i + 1]])
  }
  cat("Model including all predictors\n")
  summary(model)
}
