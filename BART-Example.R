options(java.parameters = "-Xmx2000m")
library(bartMachine)

library(MASS)
data(Boston)
X = Boston
y = X$medv
X$medv = NULL

Xtrain = X[1 : (nrow(X) / 2), ]
ytrain = y[1 : (nrow(X) / 2)]
Xtest = X[(nrow(X) / 2 + 1) : nrow(X), ]
ytest = y[(nrow(X) / 2 + 1) : nrow(X)]

set_bart_machine_num_cores(4)
bart_machine = build_bart_machine(Xtrain, ytrain,
		num_trees = 200,
		num_burn_in = 300,
		num_iterations_after_burn_in = 1000,
		use_missing_data = TRUE,
		debug_log = TRUE,
		verbose = TRUE)
bart_machine

plot_y_vs_yhat(bart_machine)

yhat = predict(bart_machine, Xtest)
# q("no")


options(java.parameters = "-Xmx1500m")
library(bartMachine)
data("Pima.te", package = "MASS")
X <- data.frame(Pima.te[, -8])
y <- Pima.te[, 8]
bart_machine = bartMachine(X, y)
bart_machine
table(y, predict(bart_machine, X, type = "class"))

raw_node_data = extract_raw_node_data(bart_machine, g = 37)
raw_node_data[[17]]



options(java.parameters = "-Xmx20000m")
library(bartMachine)
set_bart_machine_num_cores(10)
set.seed(1)
data(iris)
iris2 = iris[51 : 150, ] #do not include the third type of flower for this example
iris2$Species = factor(iris2$Species)  
X = iris2[ ,1:4]
y = iris2$Species



bart_machine = bartMachine(X, y, num_trees = 50, seed = 1)
bart_machine
##make probability predictions on the training data
p_hat = predict(bart_machine, iris2[ ,1:4])
p_hat