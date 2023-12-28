#The prophet model is a time series model created by facebook(meta) in 2017

library(quantmod)
library(prophet)
library(ggplot2)

#Download the date
symbol <- "AAPL"
start_date <- "2022-01-01"
end_date <- Sys.Date()  # Current date

getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)

price_data <- Cl(get(symbol))
date_column <- index(price_data)

# Create a data frame with date and prices
your_data <- data.frame(Date = date_column, Price = as.numeric(price_data))

head(your_data)

#rename the columns to make ready for the prophet model
colnames(your_data) <- c("ds", "y")

#divide the model into train and test

sample_size = floor(0.80 * nrow(your_data))
set.seed(109)
train_indices = sample(seq_len(nrow(your_data)) , size = sample_size)

train <- your_data[train_indices, ]
test <-  your_data[-train_indices, ]


#the prophet model
prophet_model <- prophet()

prophet_model <- fit.prophet(prophet_model, train)

future <- make_future_dataframe(prophet_model, periods = 70)  # Adjust the number of periods as needed
forecast <- predict(prophet_model, future)
plot(prophet_model, forecast)


#calculate metrics
predicted_values <- forecast$yhat[-train_indices]

actual_values <- your_data$y[-train_indices]

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predicted_values - actual_values))
print(paste("Mean Absolute Error (MAE):", mae))

#RMSE
rmse <- sqrt(mean((predicted_values - actual_values)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))


#plot predicted values vs actual values

dates <- your_data$ds[-train_indices]


actual_values <- your_data$y[-train_indices]
predicted_values <- forecast$yhat[-train_indices]

min_length <- min(length(dates), length(actual_values), length(predicted_values))
dates <- dates[1:min_length]
actual_values <- actual_values[1:min_length]
predicted_values <- predicted_values[1:min_length]

actual_data <- data.frame(ds = dates, y = actual_values, type = "Actual")

predicted_data <- data.frame(ds = dates, y = predicted_values, type = "Predicted")

combined_data <- rbind(actual_data, predicted_data)

ggplot(combined_data, aes(x = ds, y = y, color = type)) +
  geom_line() +
  labs(title = "Actual vs Predicted Values", x = "Date", y = "Value") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()