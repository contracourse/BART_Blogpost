library(bartMachine)
options(java.parameters = "-Xmx2g")
library(data.table)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)

# download ETF data from YahooFinance
stock_namelist <- c("SPY", "VGK", "EWJ", "EEM", "VNQ", "RWX", "TLT", "DBC", "GLD", "VWO", "BND")
prices <- xts()
for (i in 1:length(stock_namelist)) {
  tmp <- Ad(getSymbols(stock_namelist[i], from = "2015-01-01", to = "2019-12-31", auto.assign = FALSE))
  tmp <- na.approx(tmp, na.rm = FALSE)  # interpolate NAs
  prices <- cbind(prices, tmp) 
}
colnames(prices) <- stock_namelist
tclass(prices) <- "Date"
# View(prices)
# prices <- as.data.frame(prices)
# write.zoo(prices, file="export.csv", row.names=FALSE,col.names=TRUE,sep=",", index.name="Date")

# compute log-returns and linear returns, normalized
X_log <- CalculateReturns(prices, "log")[-1] 
X_lin <- CalculateReturns(prices)[-1] 
X_log <- as.data.table(X_log) # transform as data.table for our ML model
N <- ncol(X_log)  # number of stocks
T <- nrow(X_log)  # number of days

plot(prices/rep(prices[1, ], each = nrow(prices)), col = rainbow12equal, legend.loc = "topleft",
     main = "Normalized prices")

boxplot(X_log$SPY)


library(caret)
y <- X_log$SPY
# y <- sign(y)*log1p(abs(y)) # log transformation
df <- within(X_log, rm(SPY))
set.seed(42)
test_inds = createDataPartition(y = 1:length(y), p = 0.2, list = F)

# #Analyse the model
# model <- lm(y ~., data=df)
# plot(model)
# par(mfrow = c(2, 2))
# length(y)

df_test = df[test_inds, ]
y_test = y[test_inds]
df_train = df[-test_inds, ]
y_train = y[-test_inds]

paste("Shape of the train data: ")
paste("Shape of the test data: ")
paste(dim(df_train))
paste(dim(df_test))

library(ggplot2)
ggplot2::qplot(X_log$SPY,
      geom="histogram",
      #binwidth=0.1,
      main="Histogram of median price",
      xlab="Median price",
      fill=I("green"),
      col=I("black"))


bart_machine = bartMachine(df_train, y_train)
summary(bart_machine)


rmse_by_num_trees(bart_machine, 
                  tree_list=c(seq(25, 75, by=5)),
                  num_replicates=3)

bart_machine <- bartMachine(df_train, y_train, num_trees=70, seed=42)
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
pd_plot(bart_machine, j = "VNQ") # Investigate the most important feature in the PD plot

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
