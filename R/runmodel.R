library(lmtest)
library(datasets)
library(MASS)
library(lmSupport)
library(roxygen2)
library(devtools)
library(stringr)

##tests
forms <- create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))
mods <- create_model_objects(forms, freeny)
assumptions_check(mods[[2]])
model_output(mods)
runmodel("y", c("lag.quarterly.revenue"), c("price.index", "income.level"), dataset=freeny)

### Extra stuff that's to be implemented later
#sort(stdres(lm(mods[[2]])), decreasing = T)
#sort(standresids, decreasing = T)
#res <- sort(abs(stdres(lm(mods[[2]]))), decreasing = T)
#res
#vartransform <- function(model){
#  shapiro.test(response)$p.value
#}

### MODEL OUTPUT
for(i in 1:(length(mods) - 1)){
  modelCompare(mods[[i]], mods[[i + 1]])
}

### Main model function
runmodel <- function(outcome, block1, ..., dataset, transform.outcome=F){
  forms <- create_formula_objects(outcome, block1, ...)
  models <- create_model_objects(forms, dataset)
  top_model <- models[[length(models)]]
  assumptions_check(top_model)
  model_output(models)
}

### Creates formulas for hierarchical models
create_formula_objects <- function(outcome, block1, ...){
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

### Creates all of the hierarchical models from the created formulas
create_model_objects <- function(formulas, dataset){
  models <- lapply(X = forms, data=freeny, lm)
}

#1: durbin/watson
#2a: partial plots, linearity
#2b: unstd predicted values vs studentized residuals, linearity
#3: unstd predicted values vs studentized residuals, homogeneity
#4: Correlations, tolerance, VIF
#5: Std residual +- 3SD, cooks D, leverage
#6: Normality of residual distribution, histogram, PP plot

assumptions_check <- function(model){
  ### GATHERING INFORMATION FOR ASSUMPTION CHECKING
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

model_output <- function(models){
  model <- models[[length(models)]]
  cat("Durbin-Watson = ", dw, "p value = ", dwp, "\n")
  cat("Partial Regression plots (all relationships should be linear):\n")
  ##partplots
  ##cat("Plot of studentized residuals (should be linear and homogenous across predicted values)\n")
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
  ##cat("Leverage Plot\n")
  levplot
  ##cat("Plot of standardized model residuals\n")
  residplot
  mean(stdres(model))
  sd(stdres(model))
  curve(dnorm(x,mean(stdres(model)), sd(stdres(model))), col="darkblue", lwd=2, add=TRUE, yaxt="n")
  cat("Normality of standardized model residuals:", " Shapiro-Wilk (p-value): ", normresids, "\n")
  ##cat("PP plot:")
  plot(ppoints(length(stdres(model))), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
  abline(0,1)
  cat("Model change statistics\n")
  ### MODEL OUTPUT
  for(i in 1:(length(models) - 1)){
    modelCompare(models[[i]], models[[i + 1]])
  }
  cat("Model including all predictors\n")
  summary(model)
}
