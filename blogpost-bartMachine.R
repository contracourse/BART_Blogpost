library(bartMachine)
options(java.parameters = "-Xmx2g")
library(data.table)
library(dplyr)
load("data_ml.RData")
# data <- fread("edhec.csv")

data_ml = as.data.table(data_ml)
data_ml <- data_ml[ date %between% c("2006-12-31","2009-12-31")]

data_ml <- data_ml[month(date)==12,]  # use only the December data
data_ml[,yr := year(date)]  # create year variable

# data_ml[,.N, keyby=c("date")]  # the number of firms for each month

# The set of predictors
X = c("Mkt_Cap_12M_Usd","Pb","Sales_Ps","Mom_11M_Usd","Vol1Y_Usd","Roa",
      "Mom_Sharp_11M_Usd","Ebit_Noa","Roe","Share_Turn_12M","Ev_Ebitda",
      "Ebitda_Margin","Asset_Turnover","Capex_Sales","Total_Debt_Capital",
      "Op_Prt_Margin")

# Model construction with ridge regression and lasso

# the sample from 2006 to 2007 for training
df_train = as.data.frame(data_ml[ yr %between% c(2006,2007),X, with=F])
y_train = data_ml[ yr %between% c(2006,2007), R12M_Usd]

# the sample from 2008 for validation
df_test = as.data.frame(data_ml[ yr == 2008, X, with=F])
y_test = data_ml[ yr == 2008, R12M_Usd]



library(caret)
y <- data_ml$R12M_Usd
y <- sign(y)*log1p(abs(y)) # log transformation
df <- within(data_ml, rm(R12M_Usd))
set.seed(42)
test_inds = createDataPartition(y = 1:length(y), p = 0.2, list = F)

#Analyse the model
model <- lm(y ~., data=df)
plot(model)
par(mfrow = c(2, 2))
length(y)

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
pd_plot(bart_machine, j = "rm") # Investigate the most important feature in the PD plot

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
