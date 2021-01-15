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

###### data extraction and data cleaning ##############################################3
features <- read.csv(file.choose()) ## reading the features dataset ##
features <- na.omit(features) ## omitting the NA values ##
stores <- read.csv(file.choose()) ## reading the stores files ##
View(stores)
stores_df <-stores ##storing the values into different stores dataframe ##
sales <- read.csv(file.choose()) ## reading the sales file ##
sales_df <- sales ## storiing the value in some oother vector ##
View(sales)
fn_df <- merge(sales_df, features, by = "Store") ## merging on the basis of primary key that is sale ##
View(fn_df)
test_df <- merge(fn_df, stores_df, by = "Store") ## merging on the basis of primary key that is sale ##
final_df <- na.omit(test_df)  ## omitting the NA values from the final data frame ##
try_df <- merge(stores_df, sales_df)

?merge

###### changing the true false values into binary values #####
final_df$IsHoliday.x[final_df$IsHoliday.x==TRUE] <- 0 ## changing the value of true as 0 ##
final_df$IsHoliday.x[final_df$IsHoliday.x==FALSE] <- 1 ## changing the value of false as 1 ##
final_df$IsHoliday.y[final_df$IsHoliday.y==TRUE] <- 0 ## changing the value of true as 0 ##
final_df$IsHoliday.y[final_df$IsHoliday.y==TRUE] <- 1 ## changing the value of false as 1 ##
head(final_df)


install.packages("DataExplorer") ## data explorer package ##
library(DataExplorer) ## data explorer library ##
plot_correlation(final_df) ## plotting the heat map ##


#LASSO presenation###############################333
x <- as.matrix(subset(test_df, 
                      select = c("Store","Dept","IsHoliday.x",
                                 "Temperature","Fuel_Price","MarkDown1",
                                 "MarkDown2","MarkDown3","MarkDown4",
                                 "MarkDown5","CPI","Unemployment","Size") )) ## selectiing the numeric and integer datatype ##
?glmnet
lasso.tr <- glmnet(x,test_df$Weekly_Sales,alpha = 1) ## glmnet function to peform lasso ##
coef(lasso.tr) ## coefficient for the lasso ##
lasso.tr


plot(lasso.tr, main = "LASSO model for Sales data", label = TRUE) ## plotting the lasso functionality ##
sink("LASSO OUTPUT.txt")
print(lasso.tr) ## printing the lasso function ##
sink()

#TIME SERIES
time_series <-  ts(try_df$Weekly_Sales, start = 2010, end = 2012, frequency = 12) ## time series for the following dataset for with yearly frequency ##
plot(time_series) ## tieme series vector ##
time_series



install.packages("tseries")
library(tseries) ## library t series ##

sales_comp <- decompose(time_series)
sales_comp$seasonal
plot(sales_comp)
?seasonal
#Seasonally adjusting the components since they are not stationary
finance_comp_seas_adjusted <- finance.ts - finance_comp$seasonal
plot(finance_comp_seas_adjusted)

diff_sales <- diff(log(time_series), differences = 3) ## diff function as data has different variance and mean 
plot(diff_sales)
plot(acf(diff_sales, lag.max = 20, plot = FALSE)) ## 0 ##
acf(diff_sales, lag.max = 20, plot = FALSE)
plot(pacf(diff_sales, lag.max = 20, plot = FALSE)) ## 0 ##
pacf(diff_sales, lag.max = 20, plot = FALSE)

install.packages("forecast")
library(forecast)

arima_sales <- arima(diff_sales, order = c(3, 0, 0)) ## arima model for the difference with p, q and d value ##library(forecast) ## library forecasting ##
ps <- forecast(arima_sales, level = c(95), h = 12) ######## forecast with 95 % confidence interval ########
plot(ps) ## plotting the forecasting arima model ##


