library(bartMachine)
library(psych)
library(data.table)
options(java.parameters = "-Xmx2g")


data <- fread("Book1.csv")
data = as.data.table(data)[,-1]
# data <- data[-.N]
View(data)

library(caret)
y <- data$SPY
df <- within(data, rm(SPY))
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

pairs.panels(data, 
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)         # If TRUE, adds confidence intervals

bart_machine = bartMachine(df_train, y_train)
summary(bart_machine)


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
paste('r2:', rsq(y_test, y_pred)) # the R-squared y-test fit with predicted 
paste('rmse:', rmse(y_test, y_pred))
cor.test(y_test, y_pred, method=c("pearson"))

# Plot the importance plot
investigate_var_importance(bart_machine, num_replicates_for_avg = 20)
pd_plot(bart_machine, j = "UNRATE") # Investigate the most important feature in the PD plot

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
