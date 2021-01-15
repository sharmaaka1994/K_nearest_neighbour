install.packages("ggfortify")
install.packages("forecast")
library(ggfortify)
library(forecast)
library(ggfortify)
library("ncvreg")
library("bigmemory")
library("Matrix")
library("biglasso")
require(data.table)
require(glmnet)
require(dplyr)
library(readr)
library(biglasso)
library(stats)
library(datasets)
library(glmnet)
library(forecast)


#########installing the data set library named as time series data library############3
install.packages("remotes") ##installing the package remotes ##
remotes::install_github("FinYang/tsdl") ## downloading the time series data library##
library(tsdl) ##using the time series data library##
library(forecast) ##using forecast library##
help(tsdl)
tsdl ##time series data library##

#######time series on production dataset############
production <- subset(tsdl,"Production") ##creating the subset for production##
production 
str(production) ## creating the string of production data##
production.ts <- production[[1]] ## taking the values into production time stamp##
production.ts
plot(production.ts)
plot(cycle(production.ts)) ## finding the cycle of production time series##
bxl <- production.ts~cycle(production.ts)
boxplot(bxl, xlab = "production of goods", main = "production with yearly frequency")


##### subtracting the seasonality of data from the real data ###########
production.ts1 <- decompose(production.ts) ## decomposing the production dataset##
plot(production.ts1) ##plotting the decomposed data ##
seasonal_production <- production.ts1$seasonal ##storing the seasonality of data into a variable ##
production_seasonally_adjusted <- production.ts - seasonal_production ## deduct the seasonal data from real data ##
plot(production_seasonally_adjusted, xlab = "seasonal component", main = "predicted vs actual") ##plotting of predicted vs actual ##

### plotting the differentinating with respect to time##################3
library(tseries) ##library of tseries##
diff_production <- diff(log(production.ts), differences = 3) ## (to find the d value) difference in the actual data taking difference quotient as 1##
plot(diff_production) ## plot the diffrentiation created with respect to time ##
plot(acf(diff_production, lag.max = 20, plot = FALSE)) ##acf function to find the q value ##
plot(pacf(diff_production, lag.max = 20, plot = FALSE)) ##pacf function to find the p value ##

## plottning with theb abline function to perform the linear regression ###
plot(production.ts) ##plotting the production dataset ##
abline(lm(production.ts~time(production.ts))) ##performing the abline function ##


####################33pefroming the arima on the difference of the production found ###############
arima_production <- arima(diff_production, order = c(3, 0, 0)) ##arima model with the help of p q and d value #####
arima_production
ps <- forecast(arima_production, level = c(95), h = 12) ######## forecast with 95 % confidence interval ########
plot(ps) #####plotting the forecasted values##


