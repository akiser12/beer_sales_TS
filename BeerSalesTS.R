# Import Beer Data
library(readxl)
BeerData <- read_excel("~/Desktop/beer_sales_TS/AnadoluEfesData.xlsx", skip = 2)

# Set Year as factor variable
BeerData$Year = as.factor(BeerData$Year)

View(BeerData)

# Basic Setup
library(smooth)
library(forecast)
library(caret)

# Remove scientific notation
options(scipen = 9999)
# Look at a summary of the data
summary(BeerData)

# Create data frame of plot data
plotdata = BeerData[,c("Beer_Consumption_lt","Avg_Beer_Price","Avg_Raki_Price", "Avg_Canned_Soft_Drink_Price", "Avg_Canned_Beer_Price", "Avg_Draft_Beer_Price","Ramadan_Index", "Czechoslovakia", "Germany", "United_Kingdom", "United_States", "France", "Others_Total")]
pairs(plotdata, lower.panel = panel.smooth, upper.panel = panel.smooth)
cor(plotdata, method = "pearson")


#--------------------------- Explanatory Models --------------------------------

# Explanatory regression model using only consumption data
regmodel_cons = lm(Beer_Consumption_lt ~ Year + Month + Avg_Beer_Price + Avg_Raki_Price 
                   + Avg_Canned_Soft_Drink_Price +  Avg_Canned_Beer_Price + 
                     Avg_Draft_Beer_Price + Ramadan_Index, data = BeerData)

summary(regmodel_cons)

# Use step function to refine model
step.model_cons = step(regmodel_cons, direction = "backward", trace = 1, k = 2)
summary(step.model_cons)
plot(step.model_cons, ask=FALSE)

# Explanatory regression model using only tourism data
regmodel_tour = lm(Beer_Consumption_lt ~ Year + Month + Ramadan_Index + Czechoslovakia + Germany + 
                     United_Kingdom + United_States + France + Others_Total, data = BeerData)
summary(regmodel_tour)     

# Use step function to refine model
step.model_tour = step(regmodel_tour, direction = "backward", trace = 1, k = 2)
summary(step.model_tour)
plot(step.model_tour, ask=FALSE)


# Complete explanatory model (with both consumption and tourism data)
regmodel_full = lm(Beer_Consumption_lt ~ Year + Month + Avg_Beer_Price + Avg_Raki_Price 
                   + Avg_Canned_Soft_Drink_Price +  Avg_Canned_Beer_Price + 
                     Avg_Draft_Beer_Price + Ramadan_Index + Czechoslovakia + Germany + 
                     United_Kingdom + United_States + France + Others_Total, data = BeerData)
summary(regmodel_full)

# Use step function to refine model
step.model_full = step(regmodel_full, direction = "backward", trace = 1, k = 2)
summary(step.model_full)
plot(step.model_full, ask=FALSE)


#---------------------------- Predictive Model ---------------------------------

# Set up training and test data
train.data = BeerData[1:72,]
test.data = BeerData[73:84,]

# Regression model for predictions 
predict_model = lm(Beer_Consumption_lt ~ Month + Avg_Beer_Price + Avg_Raki_Price 
                   + Avg_Canned_Soft_Drink_Price +  Avg_Canned_Beer_Price + 
                     Avg_Draft_Beer_Price + Ramadan_Index + Czechoslovakia + Germany + 
                     United_Kingdom + United_States + France + Others_Total, data = train.data)
summary(predict_model)

# Use step function to refine model
predict_model.step = step(predict_model, direction = "backward", trace = 1, k = 2)
summary(predict_model.step)

plot(predict_model.step, ask = FALSE)

# Generate predictions 
test_predictions = predict(predict_model.step, test.data)
test_predictions
summary(test_predictions)

# Display 95% prediction interval
predict(predict_model.step, test.data, interval = "prediction", level = 0.95)

# Calculate rmse for model
rmse = sqrt(mean((test.data$Beer_Consumption_lt - test_predictions)^2))
rmse

# ----------------------------------Time Series---------------------------------

time_series <- ts(BeerData[,3], start = 1987, frequency = 12)
plot(time_series)

#Creation and plotting of the training data ranging from 1999 to 2000.
beer_training <- window(time_series, start=1987, end = c(1992, 12), frequency = 12)
plot(beer_training)
beer_training

#Creation and plotting of testing data ranged from 2001 till December of 2001.
beer_testing <- window(time_series, start = c(1993, 1), end = c(1993, 12), frequency = 12)
plot(beer_testing)
beer_testing

#Holt winters model with multiplicative seasonality
beer_hw_training <- hw(beer_training, h=12, seasonal = "multiplicative")
beer_hw_training
plot(beer_hw_training)

accuracy(beer_hw_training, beer_testing)
summary(beer_hw_training)

#Regression Time Series
#Creation of time_series2, beer_training2, and beer_testing2
time_series2 <- time_series
beer_training2 <- beer_training
beer_testing2 <- beer_testing

beer_regression_ts <- tslm(beer_training2~trend+season)
summary(beer_regression_ts)

beer_results <- forecast(beer_regression_ts, h=12)
beer_results

accuracy(beer_results, beer_testing2)

#Looking at accuracy of both our HW model and linear time series.
#HW performs better
accuracy(beer_hw_training, beer_testing)
accuracy(beer_results, beer_testing2)


