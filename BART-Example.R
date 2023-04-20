library(bartMachine)
# options(java.parameters = "-Xmx2g")
data(automobile)
automobile = na.omit(automobile)
View(automobile)
str(automobile)
y <- automobile$log_price
X <- automobile; X$log_price <- NULL

bart_machine <- bartMachine(X, y)
bart_machine    
# Note that the “p-val for shapiro-wilk test of normality of residuals” is marginally less than
#5%. Thus we conclude that the noise of Equation 1 is not normally distributed.

k_fold_cv(X, y, k_folds = 10)
rmse_by_num_trees(bart_machine, 
                  tree_list=c(seq(25, 75, by=5)),
                  num_replicates=3)

bart_machine_cv <- bartMachineCV(X, y)

k_fold_cv(X, y, k_folds = 10, k = 2, nu = 3, q = 0.9, num_trees = 200)

predict(bart_machine_cv, X[1 : 7, ])

check_bart_error_assumptions(bart_machine_cv)

plot_convergence_diagnostics(bart_machine_cv)

calc_credible_intervals(bart_machine_cv, X[100, ], ci_conf = 0.95)

calc_prediction_intervals(bart_machine_cv, X[100, ], pi_conf = 0.95)

plot_y_vs_yhat(bart_machine_cv, credible_intervals = TRUE)
plot_y_vs_yhat(bart_machine_cv, prediction_intervals = TRUE)
plot_convergence_diagnostics(bart_machine_cv)

investigate_var_importance(bart_machine_cv, num_replicates_for_avg = 20)

cov_importance_test(bart_machine_cv, covariates = c("width"))
cov_importance_test(bart_machine_cv, covariates = c("body_style"))
cov_importance_test(bart_machine_cv, covariates = c("width",
"curb_weight", "city_mpg", "length", "horsepower", "body_style",
"engine_size", "highway_mpg", "peak_rpm", "normalized_losses"))
cov_importance_test(bart_machine_cv)