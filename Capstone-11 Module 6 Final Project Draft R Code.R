#Final Project: Initial Analysis Report

# Loading necessary libraries
library(psych)
library(tidyverse)
library(ggplot2)

#Importing the Dataset 
Energy_Usage <- read.csv("Energy Usage Dataset.csv")
View(Energy_Usage)

# Understanding the Dataset
describe(Energy_Usage)
summary(Energy_Usage)
str(Energy_Usage)

# Checking for missing values
missing_values <- colSums(is.na(Energy_Usage))
print(missing_values[missing_values > 0])

# Checking for duplicates
Energy_Usage <- Energy_Usage %>% distinct()

# Loading necessary libraries
library(tidyverse)

# Reordering the months to ensure chronological order
monthly_energy_df$Month <- factor(monthly_energy_df$Month, 
                                  levels = c("January", "February", "March", "April", 
                                             "May", "June", "July", "August", 
                                             "September", "October", "November", "December"))

# Plotting the monthly total energy consumption as a bar chart with correct month order
ggplot(monthly_energy_df, aes(x = Month, y = Total_KWH, fill = Month)) + 
  geom_bar(stat = "identity", show.legend = FALSE, color = "black", alpha = 0.8) + 
  labs(title = "Total Energy Consumption by Month", 
       x = "Month", 
       y = "Total Energy Consumption (KWH)") + 
  theme_minimal(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Bar plot of Total Energy Consumption by Building Type 

ggplot(Energy_Usage, aes(x = BUILDING.TYPE, y = TOTAL.KWH, fill = BUILDING.TYPE)) + 
  geom_bar(stat = "identity", alpha = 0.8) + 
  theme_minimal(base_size = 14) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Total Energy Consumption by Building Type",  
       x = "Building Type",  
       y = "Total Energy Consumption (KWH)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Q 1- Method: Generalized Linear Model using Lasso(L1) Regularization
# Changing the categorical variable
Energy_Usage$BUILDING.TYPE <- as.factor(Energy_Usage$BUILDING.TYPE)
energy_data <- model.matrix(~ BUILDING.TYPE + AVERAGE.BUILDING.AGE + OCCUPIED.UNITS.PERCENTAGE - 1, data = Energy_Usage)

# Setting the target variable
response <- Energy_Usage$TOTAL.KWH

# Splitting the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(energy_data), size = 0.8 * nrow(energy_data))
train_data <- energy_data[train_indices, ]
train_response <- response[train_indices]

test_data <- energy_data[-train_indices, ]
test_response <- response[-train_indices]

## install.packages("glmnet")
library(glmnet)

# Confirming the train_data is a numeric matrix
train_data_matrix <- as.matrix(data.frame(train_data))

# Applying Lasso Regression
lasso_model <- cv.glmnet(train_data_matrix, train_response, alpha = 1)

# Best lambda value
best_lambda <- lasso_model$lambda.min

# Ensuring test_data is a numeric matrix
test_data_matrix <- as.matrix(data.frame(test_data))

# Generating predictions
predictions <- predict(lasso_model, s = best_lambda, newx = test_data_matrix)

# Ensuring predictions and actual responses are numeric
predictions <- as.numeric(predictions)
test_response <- as.numeric(test_response)

# Calculating the Root Mean Squared Error (rmse)
rmse <- sqrt(mean((test_response - predictions)^2))

# Calculating R-squared
r_squared <- 1 - sum((test_response - predictions)^2) / sum((test_response - mean(test_response))^2)

# Output of the results
cat("Root Mean Squared Error (rmse):", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Displaying the coefficients
coef(lasso_model, s = best_lambda)

# Installing the plotly for interactivity
##install.packages("plotly")
library(plotly)
library(ggplot2)

# Plotting the cross-validation error vs lambda
cv_results <- data.frame(lambda = lasso_model$lambda, cvm = lasso_model$cvm)

interactive_plot <- ggplot(cv_results, aes(x = log(lambda), y = cvm)) + 
  geom_line(color = "#1f77b4", size = 1.2) + 
  geom_vline(xintercept = log(best_lambda), linetype = "dashed", color = "#d62728", size = 1) + 
  labs(title = "Lasso Regression: Cross-Validation Error vs Lambda", 
       x = "Log(Lambda)", 
       y = "Mean Cross-Validation Error") + 
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))
plotly::ggplotly(interactive_plot)

# Q 2- Method: Spearman Correlation Coefficient 
# Spearman Correlation Analysis

# Selecting relevant columns for correlation
correlation_data <- Energy_Usage %>% 
  select(TOTAL.KWH, OCCUPIED.HOUSING.UNITS)

# Calculating Spearman Correlation
spearman_correlation <- cor(correlation_data$TOTAL.KWH, correlation_data$OCCUPIED.HOUSING.UNITS, method = "spearman")

# Output of Spearman Correlation Result
cat("Spearman Correlation between Total Energy Consumption and Occupied Housing Units:", spearman_correlation, "\n")

# Visualizing the Relationship
ggplot(Energy_Usage, aes(x = OCCUPIED.HOUSING.UNITS, y = TOTAL.KWH)) + 
  geom_point(alpha = 0.6, color = "#1f77b4") + 
  geom_smooth(method = "lm", se = FALSE, color = "#d62728", linetype = "dashed") + 
  labs(title = "Relationship between Total Energy Consumption and Occupied Housing Units", 
       x = "Occupied Housing Units", 
       y = "Total Energy Consumption (KWH)") + 
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))

# Q 3- Method: Kruskal-Wallis Test 
# Reshaping the data to long format for easier seasonal comparison
library(tidyverse)
library(dplyr)
energy_long <- Energy_Usage %>% 
  select(KWH.JANUARY.2010, KWH.FEBRUARY.2010, KWH.MARCH.2010, 
         KWH.APRIL.2010, KWH.MAY.2010, KWH.JUNE.2010, 
         KWH.JULY.2010, KWH.AUGUST.2010, KWH.SEPTEMBER.2010, 
         KWH.OCTOBER.2010, KWH.NOVEMBER.2010, KWH.DECEMBER.2010) %>% 
  pivot_longer(cols = everything(), names_to = "Month", values_to = "KWH")

# Categorizing the seasons
energy_long$Season <- case_when(
  energy_long$Month %in% c("KWH.JUNE.2010", "KWH.JULY.2010", "KWH.AUGUST.2010") ~ "Summer",
  energy_long$Month %in% c("KWH.DECEMBER.2010", "KWH.JANUARY.2010", "KWH.FEBRUARY.2010") ~ "Winter",
  TRUE ~ "Other"
)

# Applying Kruskal-Wallis Test
kruskal_result <- kruskal.test(KWH ~ Season, data = energy_long)

# Output of the Kruskal-Wallis Test result
cat("Kruskal-Wallis Test Result:\n")
cat("Test Statistic:", kruskal_result$statistic, "\n")
cat("Degrees of Freedom:", kruskal_result$parameter, "\n")
cat("P-value:", kruskal_result$p.value, "\n")

# Visualizing Seasonal Trends
# Load required libraries
library(ggplot2)

# Visualizing Seasonal Trends with distinct colors
ggplot(energy_long, aes(x = Season, y = KWH, fill = Season)) + 
  geom_boxplot(alpha = 0.7,) + 
  scale_fill_manual(values = c("Other" = "red",   # Light Red for Other
                               "Summer" = "green",  # Light Green for Summer
                               "Winter" = "blue"   # Light Blue for Winter
  )) +
  labs(title = "Seasonal Variation in Energy Consumption", 
       x = "Season", 
       y = "Energy Consumption (KWH)") + 
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))

# Q. 4- Time Series Forecasting with ARIMA and Exponential Smoothing

# Load necessary libraries
install.packages("forecast")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("gridExtra")

library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Summing up energy consumption per month
monthly_kwh <- colSums(Energy_Usage %>% select(starts_with("KWH.")), na.rm = TRUE)

# Converting the monthly data to time series
energy_ts <- ts(monthly_kwh, start = c(2010, 1), frequency = 12)

# Defining seasonal months
summer_months <- c(6, 7, 8)   # June, July, August
winter_months <- c(12, 1, 2)  # December, January, February
other_months <- c(3, 4, 5, 9, 10, 11)  # Other months

# Extracting energy consumption for each season
summer_kwh <- monthly_kwh[summer_months]
winter_kwh <- c(monthly_kwh[12], monthly_kwh[1], monthly_kwh[2])
other_kwh <- monthly_kwh[other_months]

# Ensuring there are enough data points
ensure_min_data <- function(kwh_values) {
  if (length(kwh_values) < 3) {
    kwh_values <- rep(mean(kwh_values, na.rm = TRUE), 3)  # Fill with mean if too short
  }
  return(kwh_values)
}

summer_kwh <- ensure_min_data(summer_kwh)
winter_kwh <- ensure_min_data(winter_kwh)
other_kwh <- ensure_min_data(other_kwh)

# Creating time series with correct frequency
summer_ts <- ts(summer_kwh, start = c(2010, 6), frequency = 12)  
winter_ts <- ts(winter_kwh, start = c(2010, 12), frequency = 12) 
other_ts <- ts(other_kwh, start = c(2010, 3), frequency = 12)  

# Applying Exponential Smoothing (ETS) models
summer_ets <- ets(summer_ts, model = "ZZZ")
winter_ets <- ets(winter_ts, model = "ZZZ")
other_ets <- ets(other_ts, model = "ZZZ")

# Forecasting with a longer horizon (h = 6)
forecast_horizon <- 6
summer_forecast <- forecast(summer_ets, h = forecast_horizon)
winter_forecast <- forecast(winter_ets, h = forecast_horizon)
other_forecast <- forecast(other_ets, h = forecast_horizon)

# Using Function to plot forecasts
plot_forecast <- function(forecast_obj, title_text) {
  autoplot(forecast_obj) +
    labs(title = title_text,
         subtitle = "Forecasted vs Actual Energy Consumption Over Time",
         x = "Month",
         y = "Energy Consumption (KWH)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"))
}

# Generating plots for each season
plot_summer <- plot_forecast(summer_forecast, "Exponential Smoothing Forecast for Summer Season")
plot_winter <- plot_forecast(winter_forecast, "Exponential Smoothing Forecast for Winter Season")
plot_other <- plot_forecast(other_forecast, "Exponential Smoothing Forecast for Other Season")

# Arranging the plots in a single output (stacked format)
grid.arrange(plot_summer, plot_winter, plot_other, ncol = 1)

