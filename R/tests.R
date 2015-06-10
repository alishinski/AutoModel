#modelz <- create_model_objects(create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level")), dataset=freeny)
#run_model("y", c("lag.quarterly.revenue"), c("price.index", "income.level"), dataset=freeny)

#create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))

#formels <- create_formula_objects("am", c("hp", "mpg"), c("disp"), c("drat"))
#moduls <- create_model_objects(formels, data=mtcars, type="binomial")
#moduls
#model_summary_table_binomial(moduls)

#head(mtcars)
#
# model <- run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars, outliers.check = "significant")
#
# install.packages("aod")
# library(aod)
#
#model <- run_model("am", c("disp", "hp"), c("cyl", "wt"), dataset = mtcars, type="binomial")
# # ?table
# ?xtabs
# xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
# xtabs()
# esoph
# DF <- as.data.frame(UCBAdmissions)
# ## Now 'DF' is a data frame with a grid of the factors and the counts
# ## in variable 'Freq'.
# DF
# ## Nice for taking margins ...
# xtable(xtabs(Freq ~ Gender + Admit, DF))
# # model$Summary
# # model$Coefficients
# model$Class_Table
# xtable()
# ?xtable
# xtabs(c(1,1,1,1,1), c(1,1,1,1,1))
# xtabs()
# library(xtable)
# # # install.packages("lordif")
# # # library(lordif)
# # runolr()
# # install.packages("BaylorEdPsych")
#pseudo <- PseudoR2(binmod)
#pseudo[1:4]
# attr(pseudo, "names")
# NagelkerkeR2(binmod)
# typeof(pseudo)
#xtable(model$SummaryDF)
#mydata <- mtcars
#fasdf <- run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars, assumptions.check = F)
#modelstuff <- run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars)
#test_mod <- lm(y ~ lag.quarterly.revenue, data = freeny)
#formula(test_mod)
#print(formula(test_mod))
#toStin
#deparse(formula(test_mod))
#RR <- "fasfd"
#RR

#mtcars
#run_model("am", c("disp", "hp"), c("drat", "wt"), dataset = mtcars, type = "binomial")
#binmod <- glm(am ~ disp + hp, data = mtcars, family = "binomial")
#
# summary(binmod)
#
#
#  wald <- wald.test(Sigma = vcov(binmod), b = coef(binmod), Terms = 3)
#  c(wald$result$chi2[1], wald$result$chi2[3])
# wald
#  # pchisq(.065, 1)
# length(attr(binmod$model, "terms"))
# # summary(binmod)
# binmod
# bhead(mtcars)
# glm()
# model1 <- lm(disp ~ hp + wt, data = mtcars)
# model2 <- lm(disp ~ hp + am + drat, data = mtcars)
# modelCompare(model1, model2)
#
# diff
#
# install.packages("rms")
# install.packages("ROCR")
# library(ROCR)
# library(rms)
# lrm(am ~ disp + hp, data = mtcars)
# predictor <- prediction(predict.glm(binmod), mtcars$am)
# prediction()
# perf1 <- performance(predictor, measure = "tpr", x.measure = "fpr")
# perf2 <- performance(predictor, measure = "acc")
# perf2
#
# cutpoint <- which.max(perf2@y.values[[1]])
# length(mtcars$am)
# binmod$model[,1]
#
# classtable <- function(model, response){
# frame <- arrange(data.frame(predict.glm(model), response), desc(predict.glm(model)))
# frame
# predictor <- prediction(predict.glm(model), response)
# accuracy <- performance(predictor, measure = "acc")
# cutpoint <- which.max(accuracy@y.values[[1]])
# cutpoint
# frame$cut <- NA
# for(i in 1:cutpoint){
#   frame$cut[i] <- 1
# }
# for(i in (cutpoint+1):nrow(frame)){
#   frame$cut[i] <- 0
# }
# predone <- frame[0:cutpoint,]
# predzero <- frame[(cutpoint + 1):nrow(frame),]
# totp <- nrow(predone)
# totn <- nrow(predzero)
# tp <- nrow(dplyr::filter(predone, response == 1))
# fp <- totp - tp
# tn <- nrow(dplyr::filter(predzero, response == 0))
# fn <- totn - tn
# specificity <- tp / totp
# sensitivity <- tn / totn
# tot_accuracy <- (tp + tn) / (totp + totn)
# tabs <- table(frame$cut, frame$response, dnn = c("Predict", "Actual"))
# print(tabs)
# cat("Specificity: ", specificity, "\n")
# cat("Sensitivity: ", sensitivity, "\n")
# cat("Total Accuracy: ", tot_accuracy, "\n")
# }
#
# classtable(binmod, mtcars$am)
#
#
# (frame, predict.glm.binmod. > 0)
# frame$mtcars.am
# count()
# tally()
# table()
# frame <- arrange(frame, desc(predict.glm.binmod.))
# frame
#
#
# table()
# library(xtable)
# xtable(tabs)
# frame
# summary(binmod)
#
# perf2@x.values[[1]][(cutpoint + 1):length(mtcars$am)]
# arrange
# mtcars$am
# tp <-
# fp <-
# tn <-
# fn <-
# tabs
# tabs[2,]
# rownames(tabs) <- c("Auto", "Man")
# colnames(tabs) <- c("Auto", "Man")
# tabs[3,] <- c(sum(tabs[,1]), sum(tabs[,2]))
# length(perf2@x.values[[1]])
# typeof(as.vector(perf1@y.values))
# diff <- perf1@y.values[[1]] - perf2@y.values[[1]]
# diff
# ratio <- perf1@y.values[[1]] / perf2@y.values[[1]]
# ratio
# rank(diff)
# rank()
# max(diff)
# max(ratio, )
# as.vec
# rank(ratio)
# xtabs()
# which.rank(29)
# plot(perf1)
# abline(a=0,b=1)
# which.max(diff)
# dist <- sqrt
# (perf1@y.values[[1]] - perf2@y.values[[1]]) ^ 2 + (perf2@y.values[[1]] - perf1@y.values[[1]]) ^ 2
#
# .76 = -1 * .05 + b
# y = - x + .81
# y = x
# 2x = .81
# x = .405
# y = .405
# .81 - .405 ^ 2 + .05 - .405 ^ 2
# perf
# xval <- perf1@x.values[[1]]
# yval <- perf1@y.values[[1]]
# intercept <- xval + yval
# point <- intercept / 2
# dist <- sqrt((xval - point) ^ 2 + (yval - point) ^ 2)
# dist
# point
# xval[17]
# yval
# intercept
# abs(perf1@y.values[[1]] + perf2@y.values[[1]]) / sqrt(2)
# install.packages("OptimalCutpoints")
# library(OptimalCutpoints)
# optimal.cutpoints(mtcars.am ~ predict.glm.binmod, status = mtcars.am, tag.healthy = 1, data = datas, methods = "ROC01")
# datas <- data.frame(mtcars$am, predict.glm(binmod))
# optimal.cutpoints(mtcars.am ~ predict.glm.binmod., status = "predict.glm.binmod.", tag.healthy = 1, data = datas, methods = "PROC01")
# optimal.cutpoints("predict.glm.binmod.", status = "mtcars.am", tag.healthy = 1, data = datas, methods = c("MaxSp", "MinValueSe", "Minimax", "PROC01", "ROC01", "CB", "MCT"))
# ?optimal.cutpoints
# datas
# perf1
# #test_mod_two <- lm(y ~ lag.quarterly.revenue + price.index, data= freeny)
# abline()
#attributes(test_mod_two)
#attr(test_mod$coefficients, "names")
#typeof(test_mod$coefficients)
#test_mod$coefficients
#typeof(as.data.frame(test_mod$coefficients))
#modcomp <- modelCompareMod(test_mod, test_mod_two)
#modelstuff$Checks$dw

#list(1:10)
