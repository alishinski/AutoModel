#modelz <- create_model_objects(create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level")), dataset=freeny)
#run_model("y", c("lag.quarterly.revenue"), c("price.index", "income.level"), dataset=freeny)

#create_formula_objects("y", c("lag.quarterly.revenue"), c("price.index", "income.level"))


#formels <- create_formula_objects("am", c("hp", "mpg"), c("disp"), c("drat"))
#moduls <- create_model_objects(formels, data=mtcars, type="binomial")
#moduls
#model_summary_table_binomial(moduls)

#head(mtcars)

#run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars)
