#modelz <- create_model_objects(create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level")), dataset=freeny)
#run_model("y", c("lag.quarterly.revenue"), c("price.index", "income.level"), dataset=freeny)

#create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))


#formels <- create_formula_objects("am", c("hp", "mpg"), c("disp"), c("drat"))
#moduls <- create_model_objects(formels, data=mtcars, type="binomial")
#moduls
#model_summary_table_binomial(moduls)

#head(mtcars)

#run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars)
#fasdf <- run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars, assumptions.check = F)
#modelstuff <- run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars)
#test_mod <- lm(y ~ lag.quarterly.revenue, data = freeny)
#formula(test_mod)
#print(formula(test_mod))
#toStin
#deparse(formula(test_mod))
#RR <- "fasfd"
#RR

#test_mod_two <- lm(y ~ lag.quarterly.revenue + price.index, data= freeny)
#attributes(test_mod_two)
#attr(test_mod$coefficients, "names")
#typeof(test_mod$coefficients)
#test_mod$coefficients
#typeof(as.data.frame(test_mod$coefficients))
#modcomp <- modelCompareMod(test_mod, test_mod_two)
#modelstuff$Checks$dw

