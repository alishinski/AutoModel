## ---- fig.width=6, fig.height=5------------------------------------------
library(AutoModel)
run_model("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"), dataset=mtcars)

## ------------------------------------------------------------------------
formulas <- create_formula_objects("mpg", c("disp", "hp"), c("cyl", "wt"), c("drat", "qsec"))
models <- create_model_objects(formulas, dataset = mtcars)
models

