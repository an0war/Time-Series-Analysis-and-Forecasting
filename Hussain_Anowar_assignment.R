# Name: Anowar Hussain
# Student Id: 151934177
# Email: anowar.hussain@tuni.fi
# assigned column is V47

# Assignment on Time Series Analysis and Forecasting

# loading the required libraries
library(TSA)
library(urca)
library(forecast)
library(tseries)
library(readr)

# Load the time series data
data <- read.csv("Case_study.csv")
# Specifying my assigned column V48 as time series data
series <- ts(data$V47)


# Step 1: Preliminary analysis of orders
#1.1

#Checking whether the dats is time series data ot not
class(series)
time(series)

# Plot the time series (Yt)
plot(series, type = "l",ylab ="Yt", xlab ="Time", main = "Time Series Plot")

# analysis of d

#display the autocorrelation function of Yt
par(mfrow=c(1,1))
acf(series, main = "ACF of Yt")
pacf(series, main = "PACF of Yt")

# Difference the series
diff_series_1 <- diff(series)
mean(diff_series_1)

diff_series_2 <- diff(diff_series_1)
mean(diff_series_2)

# Plot ACFs and PACFs after difference the series
par(mfrow=c(1,1))
acf(diff_series_1, main = "ACF of ∇Yt (k=1)")
pacf(diff_series_1, main = "PACF of ∇Yt (k=1)")

acf(diff_series_2, main = "ACF of ∇Yt (k=2)")
pacf(diff_series_2, main = "PACF of ∇Yt (k=2)")


# Augmented Dickey-Fuller test
adf.test(series)
adf.test(diff_series_1)
adf.test(diff_series_2)

### according to all the above result I suggest the value of d=1


# Analysis of (p, q)

# ACF and PACF plots after first difference the series
acf_diff <- acf(diff_series_1, main = "ACF of differenced series", lag.max = 20)
pacf_diff <- pacf(diff_series_1, main = "PACF of differenced series", lag.max = 20)

# Determine upper bounds for p and q
pmax <- which.max(acf_diff$acf[-1] ^ 2)
pmax
qmax <- which.max(pacf_diff$acf[-1] ^ 2)
qmax

# Display upper bounds
cat("Upper bound for p (AR):", pmax, "\n")
cat("Upper bound for q (MA):", qmax, "\n")

# Fit ARIMA model with determined p and q
arima_model <- auto.arima(series, d = 1, max.p = pmax, max.q = qmax, stepwise = TRUE, approximation = FALSE)
summary(arima_model)



# Step 2: Estimation and selection of ARIMA modls

# determine the value of d, pmax, qmax
d <- 1
pmax <- 4
qmax <- 4

# Initialize an empty data frame to store the results
results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric())

# Loop over all combinations of p and q within the specified bounds
for (p in 1:pmax) {
  for (q in 1:qmax) {
    # Fit the ARIMA model to the data
    model <- Arima(series, order = c(p, d, q), include.constant = FALSE)
    
    # Store the results
    results <- rbind(results, data.frame(p = p, q = q, AIC = AIC(model), BIC = BIC(model)))
  }
}

# Display the results
results

# Sort the results by AIC and BIC
sorted_results <- results[order(results$AIC, results$BIC),]
sorted_results

# Print the best three models based on AIC
best_by_aic <- head(results[order(results$AIC), ], 3)
print(best_by_aic)

# Print the best three models based on BIC
best_by_bic <- head(results[order(results$BIC), ], 3)
print(best_by_bic)




# Step:3 Diagnostic tests


# Define the diagnostic checking function
run_diagnostics <- function(model) {
  #LjungBox test for residuals
  print(Box.test(model$residuals, lag=10, type="Ljung-Box"))
  
  #ACF and PACF plots for residuals
  acf(model$residuals, main="ACF of Residuals")
  pacf(model$residuals, main="PACF of Residuals")
  
  #histogram test for residuals
  hist(model$residuals, main="Histogram of Residuals", xlab="Residuals")
  
  #Q-Q plot for residuals
  qqnorm(model$residuals)
  qqline(model$residuals)
  
  #Shapiro normality test for residuals
  print(shapiro.test(model$residuals))
}

# Run diagnostics for the best model based on AIC
best_model_aic <- Arima(series, order=c(4,1,4), include.constant=FALSE)
run_diagnostics(best_model_aic)

# Run diagnostics for the best two models based on BIC
best_model_bic1 <- Arima(series, order=c(2,1,1), include.constant=FALSE)
run_diagnostics(best_model_bic1)

best_model_bic2 <- Arima(series, order=c(1,1,2), include.constant=FALSE)
run_diagnostics(best_model_bic2)

#based on diagnostics and parsimony the best fitted model is best_model_bic2 with order (1, 1, 2)


# plot original time series and best-fitted model
plot(series, main="Original Series vs. Best-Fitted Model",xlab = "Time", ylab = "Value")
lines(fitted(best_model_bic2), col="blue") 

# Step 4: Forecast
#Fit the preferred ARIMA model
preferred_model <- Arima(series, order =c(1,1,2), include.constant = FALSE)

#Generate forecast
forecast_10 <- forecast(preferred_model, h = 10, level = 95)
forecast_25 <- forecast(preferred_model, h = 25, level = 95)

#Plot the original series with forecasts and confidence intervals for h = 10
plot(forecast_10, main = "10-step forecast with 95% CI", xlab = "Time", ylab = "Value")
lines(forecast_10$mean, col = "blue")
lines(forecast_10$lower, col = "red", lty = 2)
lines(forecast_10$upper, col = "red", lty = 2)


#Plot the original series with forecasts and confidence intervals for h = 10
plot(forecast_25, main = "25-step forecast with 95% CI", xlab = "Time", ylab = "Value")
lines(forecast_25$mean, col = "blue")
lines(forecast_25$lower, col = "red", lty = 2)
lines(forecast_25$upper, col = "red", lty = 2)
