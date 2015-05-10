library(lmtest)
library(datasets)
library(MASS)
library(lmSupport)
library(roxygen2)
library(devtools)
library(stringr)
install.packages("devtools")
install.packages("git2r")
library(lintr)

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

#1: durbin/watson
#2a: partial plots, linearity
#2b: unstd predicted values vs studentized residuals, linearity
#3: unstd predicted values vs studentized residuals, homogeneity
#4: Correlations, tolerance, VIF
#5: Std residual +- 3SD, cooks D, leverage
#6: Normality of residual distribution, histogram, PP plot
