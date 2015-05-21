#' Automated Multiple Regression Modelling
#'
#' @param outcome The dependent variable of the hierarchical model
#' @param block1 A character vector, with names of variables. The first block of independent variables.
#' @param ...  A character vector, with names of variables. Subsequent blocks of independent variables.
#' @param dataset A data frame containing variables refered to in \code{formulas}, passed to data argument of \code{lm}
#' @param transform.outcome A boolean. If TRUE, a variable transformation of the outcome is substituted in the final model if outcome is non-normal.
#' @details Calls other functions to generate model objects and test them, given specified model parameters and other options.  Formatted output is produced via \code{model_output}
#' @examples
#' \dontrun{runmodel("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))}
#' @export
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
#' @return A list of \code{lm} formulas
#' @examples
#' \dontrun{create_formula_objects("y", c("lag.quarterly.revenue"))}
#' \dontrun{create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))}
#' @export
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
    list(formula)
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
#' \dontrun{create_model_objects(create_formula_objects("y", c("lag.quarterly.revenue")), dataset = freeny)}
#' \dontrun{create_model_objects(freeny_model_formulas, dataset = freeny)}
#' @export
create_model_objects <- function(formulas, dataset){
  # Creates all of the hierarchical models from a set of formulas created with create_formula_objects
  if (length(formulas) < 2) stop("Your model contains just one block and is not hierarchical, consider lm")
  models <- lapply(X = formulas, data=dataset, lm)
}

#' Multiple Regression Assumption Checking
#'
#' @param model A \code{lm} model object.  \code{run_model} automatically calls this function for the model with all blocks of predictors included.
#' @details Creates objects related to multiple regression assumption checking.  These objects are used by \code{model_output} to produce readable output.
#' @examples
#' \dontrun{assumptions_check(freeny_model)}
#' @export
assumptions_check <- function(model){
  # Creates objects needed for assumption checking and output printing
  dw <<- lmtest::dwtest(model)$statistic
  dwp <<- lmtest::dwtest(model)$p.value
  #partplots <- avPlots(model)
  residplot <<- plot(predict(model), MASS::studres(model), main="Residuals by Predicted value", xlab="Unstandardized Predicted Values", ylab="Studentized Residuals")
  cormat <<- cor(data.frame(lapply(model.frame(model), as.numeric)), use="pairwise.complete.obs")
  vifs <<- car::vif(model)
  standresids <<- MASS::stdres(model)
  cdists <<- cooks.distance(model)
  levplot <<- car::leveragePlots(model)
  #residplot <<- hist(standresids, prob=T, breaks = 30, main = "Plot of Std Residuals", xlab="Std Residuals")
  normresids <<- shapiro.test(standresids)$p.value
  probDist <<- pnorm(MASS::stdres(model))
}

#' Multiple Regression Assumption Checking
#'
#' @param models A list of \code{lm} model objects.  A set of model objects created by \code{create_model_object}.
#' @details Creates plots and text output to summarize models and check assumptions via objects created by \code{assumptions_check}.  Uses full model with all predictors.
#' @examples
#' \dontrun{model_check(freeny_models)}
#' @export
model_output <- function(models){
  # Produces plots and prints relevant messages and outputs.
  model <- models[[length(models)]]
  cat("\n\nREGRESSION OUTPUT\n\n")
  cat("Durbin-Watson = ", dw, "p value = ", dwp, "\n\n")
  cat("Partial Regression plots (all relationships should be linear):\n\n")
  residplot
  cat("Correlation Matrix for model (correlation >.70 indicates severe multicollinearity)\n\n")
  print(cormat)
  cat("\nVariance inflation factor (<10 desired):\n\n")
  print(vifs)
  cat("\nStandardized Residuals (observations > 3.00 problematic):\n\n")
  res <- sort(abs(standresids), decreasing = T)
  print(res)
  cat("\nCook's distance (values >.2 problematic):\n\n")
  print(sort(cdists, decreasing = T))
  levplot
  #residplot
  hist(standresids, prob=T, breaks = 30, main = "Plot of Std Residuals", xlab="Std Residuals")
  distmean <- mean(MASS::stdres(model))
  distsd <- sd(MASS::stdres(model))
  #curve(dnorm(x, distmean, distsd, col="darkblue", lwd=2, add=TRUE, yaxt="n"))
  cat("\nNormality of standardized model residuals:", " Shapiro-Wilk (p-value): ", normresids, "\n\n")
  plot(ppoints(length(MASS::stdres(model))), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
  abline(0,1)
  cat("Model change statistics\n\n")
  # Compares the models between each block of predictors
  model_summary_table(models)
  cat("\nModel Coefficients\n\n")
  model_coefficient_table(models)
}

#' Hierarchical regression: model summary output
#'
#' @param models A list of \code{lm} model objects.  A set of model objects created by \code{create_model_object}.
#' @details Creates table output to summarize model statistics for all models in a hierarchical regression analysis.
#' @examples
#' \dontrun{model_summary_table(freeny_models)}
#' @export
model_summary_table <- function(models) {
  model_para_list <- list()
  model_para_list[[1]] <- list(
  summary(models[[1]])$r.squared,
  sqrt(summary(models[[1]])$r.squared),
  summary(models[[1]])$adj.r.squared,
  summary(models[[1]])$sigma,
  summary(models[[1]])$r.squared,
  as.numeric(summary(models[[1]])$fstatistic[1]),
  as.numeric(summary(models[[1]])$fstatistic[2]),
  as.numeric(summary(models[[1]])$fstatistic[3]),
  1 - pf(
  as.numeric(summary(models[[1]])$fstatistic[1]),
  as.numeric(summary(models[[1]])$fstatistic[2]),
  as.numeric(summary(models[[1]])$fstatistic[3])
  )
  )
  for(i in 2:length(models)) {
    comparison <- modelCompareMod(models[[i-1]], models[[i]], printOutput = F)
    model_para_list[[(length(model_para_list) + 1)]] <- list(
    R2_1 = summary(models[[i]])$r.squared,
    R_1 = sqrt(summary(models[[i]])$r.squared),
    R2_Adj_1 = summary(models[[i]])$adj.r.squared,
    SE_1 = summary(models[[i]])$sigma,
    delta12 = comparison$DeltaR2,
    F12 = comparison$Fstat,
    DF1_2 = as.numeric(comparison$nDF),
    DF2_2 = as.numeric(comparison$dDF),
    F12p = comparison$p
  )
  }
  output_matrix <- matrix(nrow=length(models), ncol=9)
  model_labels <- list()
  for(i in 1:length(models)){
    output_matrix[i,] <- unlist(model_para_list[[i]])
    model_labels[length(model_labels) + 1] <- as.character(paste("Model", i))
  }
  colnames(output_matrix) <- c("R", "R^2", "Adj R^2", "SE Est.", "Delta R^2", "F Change", "df1", "df2", "Sig F Change")
  rownames(output_matrix) <- unlist(model_labels)
  print(output_matrix, digits = 3)
}

#' Hierarchical regression: Coefficient table output
#'
#' @param models A list of \code{lm} model objects.  A set of model objects created by \code{create_model_object}.
#' @details Creates table output to summarize model coefficients for all models in a hierarchical regression analysis.
#' @examples
#' \dontrun{model_coefficient_table(freeny_models)}
#' @export
model_coefficient_table <- function(models){
  tables <- broom::tidy(models[[1]])
  tables <- cbind(Model = "Model 1", tables)
  for(i in 2:length(models)){
    tables <- rbind(tables, cbind(Model = paste("Model", i), broom::tidy(models[[i]])))
  }
  print(tables, row.names = F)
}

#' Modified modelCompare function from lmSupport
#'
#' @param ModelC A model \code{lm} object.
#' @param ModelA A model \code{lm} object.
#' @details This is a modification of the modelCompare function that allows print output to be suppressed.
#' @export
modelCompareMod <- function(ModelC, ModelA, printOutput = T) {
    sseC = sum(residuals(ModelC)^2)
    sseA = sum(residuals(ModelA)^2)

    pC = length(coef(ModelC))
    pA = length(coef(ModelA))
    if (!(pA > pC))  stop('Invalid model comparison:  modelA does not have more parameters than modelC')

    #Added the next three lines to check whether the terms in model C are a subset of the terms in model A
    termsC <- attr(terms(ModelC), "term.labels")
    termsA <- attr(terms(ModelA), "term.labels")

    if (!all(termsC %in% termsA))  stop('Invalid model comparison:  modelC is not a subset of modelA')

    nC = ModelC$df.residual + pC
    nA = ModelA$df.residual + pA
    if (!(nC == nA))  stop('Invalid model comparison:  ModelA and ModelC have different N')

    nDF = pA - pC
    dDF = nA - pA
    FStat=   ((sseC - sseA) / (pA-pC)) / (sseA / (nA-pA))

    p = pf(FStat,nDF, dDF, lower.tail = FALSE)

    PRE = (sseC - sseA) / sseC
    DeltaR2 = summary(ModelA)$r.squared - summary(ModelC)$r.squared

    #print output
    if(printOutput == TRUE){
    cat('SSE (Compact) = ', sseC, '\n', sep=' ')
    cat('SSE (Augmented) = ', sseA,  '\n', sep=' ')
    cat('Delta R-Squared = ', DeltaR2,  '\n', sep=' ')
    cat('Partial Eta-Squared (PRE) = ', PRE,  '\n', sep=' ')
    cat('F(', nDF, ',', dDF, ') = ', FStat, ', ', 'p = ', p, '\n', sep='')
    }
    Results = list(sseC=sseC, sseA=sseA, pC=pC, pA=pA, nDF=nDF, dDF=dDF, Fstat=FStat, p=p,  PRE=PRE, DeltaR2=DeltaR2)
    invisible(Results)  #return but dont print list
  }
