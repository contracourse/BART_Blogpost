library(mlbench)
library(bartMachine)
data(BostonHousing)
dim(BostonHousing)
options(java.parameters = "-Xmx2g")

library(caret)
y <- BostonHousing$medv
df <- within(BostonHousing, rm(medv))
set.seed(42) 
test_inds = createDataPartition(y = 1:length(y), p = 0.2, list = F) 

df_test = df[test_inds, ]
y_test = y[test_inds]
df_train = df[-test_inds, ]
y_train = y[-test_inds]

paste("Shape of the train data: ")
paste("Shape of the test data: ")
paste(dim(df_train))
paste(dim(df_test))

library(ggplot2)
ggplot2::qplot(y, 
      geom="histogram",
      #binwidth=0.1,
      main="Histogram of median price",
      xlab="Median price",
      fill=I("green"),
      col=I("black"))


bart_machine = bartMachine(df_train, y_train)
summary(bart_machine)

pd_plot(bart_machine, j = "zn")
rmse_by_num_trees(bart_machine, 
                  tree_list=c(seq(25, 75, by=5)),
                  num_replicates=3)

bart_machine <- bartMachine(df_train, y_train, num_trees=65, seed=42)
plot_convergence_diagnostics(bart_machine)

check_bart_error_assumptions(bart_machine)

plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)
plot_y_vs_yhat(bart_machine, Xtest=df_test, ytest=y_test, prediction_intervals = TRUE)

rmse <- function(x, y) sqrt(mean((x - y)^2))
rsq <- function(x, y) summary(lm(y~x))$r.squared
y_pred <- predict(bart_machine, df_test)
paste('r2:', rsq(y_test, y_pred))
paste('rmse:', rmse(y_test, y_pred))
cor.test(y_test, y_pred, method=c("pearson"))

investigate_var_importance(bart_machine, num_replicates_for_avg = 20)


# Example aus BART
set.seed(11)
n = 200
p = 5
X = data.frame(matrix(runif(n * p), ncol = p))
y = 10 * sin(pi* X[ ,1] * X[,2]) +20 * (X[,3] -.5)^2 + 10 * X[ ,4] + 5 * X[,5] + rnorm(n)
##build BART regression model
bart_machine = bartMachine(X, y)
#get posterior distribution
posterior = bart_machine_get_posterior(bart_machine, X)
print(posterior$y_hat)
plot_convergence_diagnostics(bart_machine)
