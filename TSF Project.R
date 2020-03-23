## Load the library forecast
library(forecast)

## Store gas dataset in to a new object
TSdata<-gas

# Let's check the class of the dataset to be sure that it is a time series data set.
class(TSdata)

# Given timeseries is univariate, has only one variable.
# Find the start of the series, end of the series, frequency and cycle.

### To print start of the series

start(TSdata)

### To print end of the series
end(TSdata)

### To print frequency of the series
frequency(TSdata)

### To print the cycle across
cycle(TSdata)


# Let's start the data exploration step with the summary function
summary(TSdata)

 
### Aggregation at a Quarter and Year Level
 
 TSdata.qtr <- aggregate(TSdata, nfrequency=4)
 TSdata.yr <- aggregate(TSdata, nfrequency=1)
 
### Plots
 plot.ts(TSdata, main = "Monthly Gas Production in Australia",
         xlab = "Time", ylab = "Gas Production")

 
 
### Quarter plot shows some clear indication of seasonality
 
 plot.ts(TSdata.qtr, main = "Quarterly Gas Production in Australia",
         xlab = "Time", ylab = "Gas Production")

### Yearly plot has clear indication of trend in the data set 
 
 plot.ts(TSdata.yr, main = "Yearly Gas Production in Australia",
         xlab = "Time", ylab = "Gas Production")

### Below are Seasonality Plot for further analysis
 
 seasonplot(TSdata, year.labels = TRUE, year.labels.left=TRUE, col=1:40, pch=19,
    main = "Monthly Gas Production in Australia - seasonplot", xlab = "Time", 
    ylab = "Gas Production")


# Monthly gas production

monthplot(TSdata, main = "Monthly Gas Production in Australia - monthplot", 
           xlab = "Time", ylab = "Gas Production")

# Box plot also shows some seasonality and this also indicates that 
# there are no outlier in the data set.


boxplot(TSdata, main = "Monthly Gas Production in Australia - boxplot", 
        xlab = "Time", ylab = "Gas Production")



  
# How to visually differentiate an additive and Multiplicative Model

decompgas = decompose(TSdata, type="additive")
plot (decompgas)


Deseason_gas <- (decompgas$trend+decompgas$random)
ts.plot(TSdata, Deseason_gas, col=c("red", "blue"), 
        main="Gas Production vs Deseasoned Gas Production")



TSdata_new <- ts(TSdata, start=c(1970,1),end=c(1995,8), frequency=12)

### Divide data into test and train

DataATrain <- window(TSdata_new, start=c(1970,1), end=c(1993,12), frequency=12)
DataATest <- window(TSdata_new, start=c(1994,1), frequency=12)



3. Phase III - Model Planning and Building
3.1 Model Planning
3.1.1 Check for stationary time series



library(tseries)
adf.test(TSdata_new)

# observe through the plot if the series is stationary or not

diff_TSdata_new <- diff(TSdata_new)
plot(diff_TSdata_new)


# Null Hypothesis is retained , hence gas data is non-stationary;

## Let's perform Dicky Fuller test on the differenced series to confirm the same.

adf.test(diff_TSdata_new)


### ACF and PACF (performing to check the stationary data and autocorrelation)

acf(diff_TSdata_new)



## To confirm if the correlation in the later lags is not because of mutual correlation,
  let's draw the partial correlation plot
  
pacf(diff_TSdata_new)

## Hence let's start with Auto Arima model.

TSdat.arima.fit.train <- auto.arima(DataATrain, seasonal=TRUE)
TSdat.arima.fit.train



## Arima
#  Let's fit the autoarima output using the Arima function

fit <- Arima(DataATrain, c(1, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
fit

  arima(x = DataATrain, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1),
                                                            period = 12))

plot(fit$x,col="blue")
lines(fit$fitted,col="red",main="Production : Actual vs Forecast")

## Box-Ljung Test


Box.test(fit$residuals, type = c("Ljung-Box"))


# Now the model is valid, 
# let's check the model performance on the train dataset

VecA1 <- cbind(fit$fitted,fit$x)
MAPEA_train <- mean(abs(VecA1[,1]-VecA1[,2])/VecA1[,1])
MAPEA_train

# Let's forecast the holdout sample using the above model. 
# Period is considered as 20 because we have 20 periods in the holdout sample

Arimafcast <- forecast(fit, h=20)
VecA2 <- cbind(DataATest,Arimafcast)
MAPEA_holdout <- mean(abs(VecA2[,1]-VecA2[,2])/VecA2[,1])
MAPEA_holdout

ts.plot(VecA2[,1],VecA2[,2], col=c("blue","red"),xlab="year", ylab="production",
        main="Production: Actual vs Forecast")

### Final Model


Final_model <- auto.arima(TSdata_new, seasonal=TRUE)
Final_model

Box.test(Final_model$residuals, type = c("Ljung-Box"))

## Now forecast for 12 months, unknown period.

Final_forecast <- forecast(Final_model, h=12)
plot(Final_forecast)

## Phase IV Next Steps for Model Refining
#  Now we have built a robust model. Can this model be refined further?
  
 



















