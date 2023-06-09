library(bartMachine)
library(data.table)
library(fredr)
library(ggplot2)
library(tidyr)
options(java.parameters = "-Xmx2g")
set_bart_machine_num_cores(3)

tibble::tibble(a = 1)
api_key <- "your-api-key"
fredr_set_key(api_key)

cpi_df <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2022-12-31"), 
  frequency = "m",
  units = "pc1" # percentage change year ago
)

library(purrr)
purrr::map_dfr(c("UNRATE", "T10Y3M", "STLFSI4", "EFFR", "T5YIFR", "REAINTRATREARAT1YE"), fredr)
# Using purrr::pmap_dfr() allows you to use varying optional parameters
params <- list(
  series_id = c("UNRATE", "T10Y3M", "STLFSI4", "EFFR", "T5YIFR", "REAINTRATREARAT1YE"),
  frequency = "m"
)
df <- purrr::pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = .x, frequency = .y)
)


df <- as.data.table(df)
df1 <- rbind(df, cpi_df) 
df1 <- df1[ date %between% c("2004-01-01","2022-12-31")]
which(is.na(df1), arr.ind=TRUE) # checking if there are NA's in the dataframe
# df_a <- df1[,2:3]

data_frame <- pivot_wider(df1, names_from = "series_id",
values_from = "value")
str(data_frame)
data_frame <- data_frame[,4:10] # delete the dates column
which(is.na(df1), arr.ind=TRUE) # checking if there are NA's in the dataframe

library(caret)
y <- data_frame$UNRATE
df <- within(data_frame, rm(UNRATE))
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

# #Looking at the variables 
# ggplot(data = df1,aes(x = date,y = value,color = series_id)) + geom_line() +
#   ylab("%") + xlab("date") + 
#   labs(title = "", 
#        caption = "Source: FRED") +
#   theme(plot.title = element_text(hjust = 0.5))


bart_machine = bartMachine(df_train, y_train)
summary(bart_machine)


rmse_by_num_trees(bart_machine, 
                  tree_list=c(seq(15, 75, by=5)),
                  num_replicates=3)

bart_machine <- bartMachine(df_train, y_train, num_trees=20)
plot_convergence_diagnostics(bart_machine)

check_bart_error_assumptions(bart_machine)

plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE)

par(mfrow = c(2,1),     # 2 rows, 1 column
    mar = c(4,4,2,1))
plot_y_vs_yhat(bart_machine, prediction_intervals = TRUE, interval_confidence_level = 0.90)
plot_y_vs_yhat(bart_machine, Xtest=df_test, ytest=y_test, credible_intervals = TRUE, interval_confidence_level = 0.90)

y_pred <- predict(bart_machine, df_test)

summary(lm(y_test~y_pred))$r.squared
sqrt(mean((y_test - y_pred)^2))
cor.test(y_test, y_pred, method=c("pearson"))

# Plot the importance plot
investigate_var_importance(bart_machine, num_replicates_for_avg = 20)
pd_plot(bart_machine, j = "EFFR") # Investigate the most important feature in the PD plot




