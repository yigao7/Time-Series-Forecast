library(lubridate)
library(tidyverse)
library(xgboost)
library(fastDummies)

# Load data in R
setwd("/Users/gaoyi/Desktop")

data = read.csv("CompetitionData.csv")

# First and last few rows of data
head(data)
tail(data)

# what are types of features/columns
str(data)

# Let's convert Date column from character to date type format
# in R there is a package called lubridate that contains functions
# to work with dates and times:
data$Date[1]
dmy(data$Date[1])
data$Date = dmy(data$Date)

str(data)

# Let's create a column that represents day of the week in data
wday(data$Date[1],label=T,abbr=F)
data$DayOfWeek = wday(data$Date,label=T,abbr=F)

# create a column that represents month in data
month(data$Date[1],label=T)
data$Month = month(data$Date,label=T)

str(data)

# Visualize data
data$Trend = 1: (dim(data)[1])
head(data)
tail(data)

data %>% ggplot(aes(x=Trend,y=Load)) + geom_line()+
  theme_bw()

# Visualize 1 day data
data %>% slice(1:(24*1)) %>%  ggplot(aes(x=Trend,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# Visualize 2 day data
data %>% slice(1:(24*2)) %>%  ggplot(aes(x=Trend,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# Visualize 7 day data
data %>% slice(1:(24*7)) %>%  ggplot(aes(x=Trend,y=Load)) + 
  geom_line()+ geom_point() + theme_bw()

# Visualize 28 day data
data %>% slice(1:(24*28)+10000) %>%  ggplot(aes(x=Trend,y=Load)) + 
  geom_line() + theme_bw()

##########################################################
#          Divide data into training and testing 
##########################################################
### Training set: 2008/1/1 – 2010/12/31
### Testing set: 2011/01/01 – 2011/12/31

data$Actual = data$Load

# replace values in Load for the testing set
data$Load[data$Date>="2011-01-01"] = NA # exclude testing set

head(data)
tail(data)

##########################################################
# M1: Linear Regression with Trend and DayOfWeek
##########################################################

M1 = lm(Load ~ Trend + DayOfWeek, data=data)
summary(M1)

# R-squared: 0.08495 

data$M1 = predict(M1,newdata=data)

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*4)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M1),col="blue")+
  theme_bw()

# Metrics of accuracy

# Training RMSE and MAPE:
test="2011-01-01"
test_end = "2011-12-31"

# RMSE training 
sqrt(mean((data$Actual[data$Date<test] - data$M1[data$Date<test])^2))
# 2167.911

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[data$Date<test] - data$M1[data$Date<test])^2))/
  mean(data$Actual[data$Date<test]) * 100
# 18.63

# MAPE training 
mean(abs((data$Actual[data$Date<test] - data$M1[data$Date<test])/
           data$Actual[data$Date<test])) *100
# 14.14%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M1[(data$Date>=test)&(data$Date<=test_end)])^2))
# 2376.444

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M1[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 14.39%

##########################################################
# M2: Linear Regression with Trend, DayOfWeek, and Hour
##########################################################
data$Hour = as.factor(data$Hour)
str(data)

M2 = lm(Load ~ Trend + DayOfWeek + Hour, data=data)
summary(M2)

# R-squared: 0.4242

data$M2 = predict(M2,newdata=data)

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*4)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M2),col="blue")+
  theme_bw()

# Metrics of accuracy
# RMSE training 
sqrt(mean((data$Actual[data$Date<test] - data$M2[data$Date<test])^2))
# 1718.938

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[data$Date<test] - data$M2[data$Date<test])^2))/
  mean(data$Actual[data$Date<test]) * 100
# 14.77174

# MAPE training 
mean(abs((data$Actual[data$Date<test] - data$M2[data$Date<test])/
           data$Actual[data$Date<test])) *100
# 11.03%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M2[(data$Date>=test)&(data$Date<=test_end)])^2))
# 1976.912

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M2[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 11.48%

##########################################################
# M3: Linear Regression with Trend, DayOfWeek, Hour, DayOfWeek*Hour
##########################################################

M3 = lm(Load ~ Trend + DayOfWeek + Hour + DayOfWeek*Hour, data=data)
summary(M3)

# R-squared: 0.4469

data$M3 = predict(M3,newdata=data)

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*4)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M3),col="blue")+
  theme_bw()

# Metrics of accuracy
# RMSE training 
sqrt(mean((data$Actual[data$Date<test] - data$M3[data$Date<test])^2))
# 1680.316

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[data$Date<test] - data$M3[data$Date<test])^2))/
  mean(data$Actual[data$Date<test]) * 100
# 14.43984

# MAPE training 
mean(abs((data$Actual[data$Date<test] - data$M3[data$Date<test])/
           data$Actual[data$Date<test])) *100
# 10.68%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M3[(data$Date>=test)&(data$Date<=test_end)])^2))
# 1946.655

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M3[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 11.12%

##########################################################
# M4: Linear Regression with Trend, DayOfWeek, Hour, DayOfWeek*Hour, Temperature
##########################################################

M4 = lm(Load ~ Trend + DayOfWeek + Hour + DayOfWeek*Hour + Temperature, data=data)
summary(M4)

# R-squared: 0.4596

data$M4 = predict(M4,newdata=data)

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*4)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M4),col="blue")+
  theme_bw()

# Metrics of accuracy
# RMSE training 
sqrt(mean((data$Actual[data$Date<test] - data$M4[data$Date<test])^2))
# 1660.89

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[data$Date<test] - data$M4[data$Date<test])^2))/
  mean(data$Actual[data$Date<test]) * 100
# 14.2729

# MAPE training 
mean(abs((data$Actual[data$Date<test] - data$M4[data$Date<test])/
           data$Actual[data$Date<test])) *100
# 11.01%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M4[(data$Date>=test)&(data$Date<=test_end)])^2))
# 1869.368

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M4[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 10.95%

############ Examine the ACF 
tsdisplay(M4$residuals,lag.max=24*7)
# Obvious cyclical pattern
# Include lag 1, 2, 24, 25, 26
data$Loadlag1 = lag(data$Load,1) 
data$Loadlag2 = lag(data$Load,2) 
data$Loadlag24 = lag(data$Load,24) 
data$Loadlag25 = lag(data$Load,25) 
data$Loadlag26 = lag(data$Load,26) 

data %>% head

##########################################################
# M5: Linear Regression with Trend, DayOfWeek, Hour, DayOfWeek*Hour, Temperature,
#     lag1 + lag2 + lag24 + lag25 + lag26
##########################################################

M5 = lm(Load ~ Trend + DayOfWeek + Hour + DayOfWeek*Hour + Temperature + 
          Loadlag1 + Loadlag2 + Loadlag24 + Loadlag25 + Loadlag26, data=data)
summary(M5)

# R-squared: 0.9976

tsdisplay(M5$residuals,lag.max=60)

# fit on train data
data$M5=NA 

data$M5[!is.na(data$Load) &!is.na(data$Loadlag1) & !is.na(data$Loadlag2) &
          !is.na(data$Loadlag24) & !is.na(data$Loadlag25)& !is.na(data$Loadlag26)] = M5$fitted.values

head(data,100)
tail(data)

# predict on test data
i=26305 # First testing observation number 
data$M5[i] = predict(M5,newdata=data[i,])

for(i in 26306:35064){
  data$Loadlag1[i] = ifelse(is.na(data$Load[i-1]),data$M5[i-1],data$Load[i-1]) 
  data$Loadlag2[i] = ifelse(is.na(data$Load[i-2]),data$M5[i-2],data$Load[i-2])
  data$Loadlag24[i] = ifelse(is.na(data$Load[i-24]),data$M5[i-24],data$Load[i-24])
  data$Loadlag25[i] = ifelse(is.na(data$Load[i-25]),data$M5[i-25],data$Load[i-25])
  data$Loadlag26[i] = ifelse(is.na(data$Load[i-26]),data$M5[i-26],data$Load[i-26])
  data$M5[i] = predict(M5,newdata=data[i,])
}

# plot data and overplay predictions (2 months)
data %>% slice(1:(24*7*8)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M5),col="blue")+
  theme_bw()

# Metrics of accuracy

# RMSE training 
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=27)] - data$M5[(data$Date<test)&(data$Trend>=27)])^2))
# 111.5184

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=27)] - data$M5[(data$Date<test)&(data$Trend>=27)])^2))/
  mean(data$Actual[(data$Date<test)&(data$Trend>=27)]) * 100
# 0.9582895

# MAPE training 
mean(abs((data$Actual[(data$Date<test)&(data$Trend>=27)] - data$M5[(data$Date<test)&(data$Trend>=27)])/
           data$Actual[(data$Date<test)&(data$Trend>=27)])) *100
# 0.63%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M5[(data$Date>=test)&(data$Date<=test_end)])^2))
# 1862.624

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M5[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 11.11%
# Looks like overfitting, so delete lag 25 and 26

##########################################################
# M6: Linear Regression with Trend, DayOfWeek, Hour, DayOfWeek*Hour, Temperature,
#     lag1 + lag2 + lag24
##########################################################
M6 = lm(Load ~ Trend + DayOfWeek + Hour + DayOfWeek*Hour + Temperature + 
          Loadlag1 + Loadlag2 + Loadlag24, data=data)
summary(M6)

# R-squared: 0.9939

# fit on train data
data$M6=NA 

data$M6[!is.na(data$Load) &!is.na(data$Loadlag1) & !is.na(data$Loadlag2) &
          !is.na(data$Loadlag24)] = M6$fitted.values

# predict on test data
i=26305 # First testing observation number 
data$M6[i] = predict(M6,newdata=data[i,])

for(i in 26306:35064){
  data$Loadlag1[i] = ifelse(is.na(data$Load[i-1]),data$M6[i-1],data$Load[i-1]) 
  data$Loadlag2[i] = ifelse(is.na(data$Load[i-2]),data$M6[i-2],data$Load[i-2])
  data$Loadlag24[i] = ifelse(is.na(data$Load[i-24]),data$M6[i-24],data$Load[i-24])
  data$M6[i] = predict(M6,newdata=data[i,])
}

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*8)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M6),col="blue")+
  theme_bw()

# Metrics of accuracy

# RMSE training 
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M6[(data$Date<test)&(data$Trend>=25)])^2))
# 176.9204

# CV = Coefficient of Variation = RMSE / mean(value)
# Rule of thumb: if CV is less than 10% then predictions are accurate
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M6[(data$Date<test)&(data$Trend>=25)])^2))/
  mean(data$Actual[(data$Date<test)&(data$Trend>=25)]) * 100
# 1.520299

# MAPE training 
mean(abs((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M6[(data$Date<test)&(data$Trend>=25)])/
           data$Actual[(data$Date<test)&(data$Trend>=25)])) *100
# 1.01%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M6[(data$Date>=test)&(data$Date<=test_end)])^2))
# 1876.107

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M6[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 11.01%

##########################################################
# M7: ARIMA with Fourier
##########################################################

train_data = data$Load[data$Date<test]
test_data  = data$Actual[(data$Date>=test)&(data$Date<=test_end)]

y.train = msts(train_data, seasonal.periods=c(24, 24*7, 24*365.25))
y.test = msts(test_data, seasonal.periods=c(24, 24*7, 24*365.25))

temp_train = ts(data$Temperature[data$Date<test])
temp_test = ts(data$Temperature[data$Date<test])

length(y.train)  
length(y.test)

M7 = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(2, 3, 8)), lambda="auto")

# generate forecast
MF7=forecast(M7, xreg=fourier(y.train,K=c(2,3,8), h=length(y.test)))

autoplot(y.train) + 
  autolayer(MF7$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF7$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF7$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 2551.27177
# MAPE = 15.74%

##########################################################
# M8: NN with Fourier
##########################################################

M8 = nnetar(y.train, seasonal = F, xreg=fourier(y.train, K = c(2, 5, 10)),MaxNWts=3241)

# generate forecast
MF8=forecast(M8, xreg=fourier(y.train,K=c(2,5,10), h=length(y.test)))

autoplot(y.train) + 
  autolayer(MF8$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF8$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF8$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 2141.175187
# MAPE = 9.76%

##########################################################
# M9: Multiple Seasonality Decomposition - ARIMA
##########################################################

M9 = mstl(y.train) 
autoplot(M9)
MF9 = forecast(M9,method="arima",h=length(y.test))

autoplot(y.train) + 
  autolayer(MF9$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF9$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF9$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 1782.876573
# MAPE = 9.31%

##########################################################
# M10: Multiple Seasonality Decomposition - ETS
##########################################################

M10 = mstl(y.train) 
autoplot(M10)
MF10 = forecast(M10,method="ets",h=length(y.test))

autoplot(y.train) + 
  autolayer(MF10$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF10$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF10$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 1735.024991
# MAPE = 8.92%

##########################################################
# M11: Multiple Seasonality Decomposition - naive
##########################################################

M11 = mstl(y.train) 
autoplot(M11)
MF11 = forecast(M11,method="naive",h=length(y.test))

autoplot(y.train) + 
  autolayer(MF11$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF11$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF11$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 1872.53829
# MAPE = 10.13%

##########################################################
# M12: Multiple Seasonality Decomposition - rwdrift
##########################################################

M12 = mstl(y.train) 
autoplot(M12)
MF12 = forecast(M12,method="rwdrift",h=length(y.test))

autoplot(y.train) + 
  autolayer(MF12$fitted, series="Fitted values")

autoplot(y.test) + 
  autolayer(msts(MF12$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF12$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 1995.52630
# MAPE = 11.28%

##########################################################
# M13: xgboost - Trend, Month, DayOfWeek, Hour, Temperature,
#                lag1 + lag2 + lag24
##########################################################

str(data)

train_data = data %>% select('Trend','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
train_data = train_data %>% slice(25:26304)

train_data = dummy_cols(train_data,select_columns = 'DayOfWeek')

train_data = train_data %>% select(!'DayOfWeek')

train_data = dummy_cols(train_data,select_columns = 'Month')

train_data = train_data %>% select(!'Month')

str(train_data)

train_matrix = data.matrix(train_data)

train_label_matrix = data.matrix(data %>% select("Load") %>% 
                             slice(25:26304))

test_data = data %>% select('Trend','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
test_data = test_data %>% slice(26305:35064)

test_data = dummy_cols(test_data,select_columns = 'DayOfWeek')

test_data = test_data %>% select(!'DayOfWeek')

test_data = dummy_cols(test_data,select_columns = 'Month')

test_data = test_data %>% select(!'Month')

str(test_data)

test_matrix = data.matrix(test_data)

test_label_matrix = data.matrix(data %>% select("Actual") %>% 
                                  slice(26305:35064))

M13 = xgboost(data = train_matrix, label = train_label_matrix,
              nround = 8,
              objective = "reg:squarederror",
              eta = .99,
              num_parallel_tree = 10) 

M13_fitted = predict(M13, train_matrix)

data$M13=NA 

data$M13[25:26304] = M13_fitted

# predict on test data
i=26305 # First testing observation number 
data$M13[i] = predict(M13, data.matrix(test_data[i-26304,]))

for(i in 26306:35064){
  data$Loadlag1[i] = ifelse(is.na(data$Load[i-1]),data$M13[i-1],data$Load[i-1]) 
  data$Loadlag2[i] = ifelse(is.na(data$Load[i-2]),data$M13[i-2],data$Load[i-2])
  data$Loadlag24[i] = ifelse(is.na(data$Load[i-24]),data$M13[i-24],data$Load[i-24])
  test_data = data %>% select('Trend','Month','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
  test_data = test_data %>% slice(26305:35064)
  test_data = dummy_cols(test_data,select_columns = 'DayOfWeek')
  test_data = test_data %>% select(!'DayOfWeek')
  test_data = dummy_cols(test_data,select_columns = 'Month')
  test_data = test_data %>% select(!'Month')
  data$M13[i] = predict(M13, data.matrix(test_data[i-26304,]))
}

# plot data and overplay predictions (1 month)
data %>% slice(1:(24*7*8)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M13),col="blue")+
  theme_bw()

# plot data and overplay predictions on testing data
data %>% slice(26306:35064) %>%  ggplot(aes(x=Trend,y=Actual)) +geom_line() +
  geom_line(aes(x=Trend,y=M13),col="blue")+
  theme_bw()

# Metrics of accuracy

# RMSE training 
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M13[(data$Date<test)&(data$Trend>=25)])^2))
# 204.5966

# MAPE training 
mean(abs((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M13[(data$Date<test)&(data$Trend>=25)])/
           data$Actual[(data$Date<test)&(data$Trend>=25)])) *100
# 1.33%

#RMSE test
sqrt(mean((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
             data$M13[(data$Date>=test)&(data$Date<=test_end)])^2))
# 830.5295

# MAPE testing
mean(abs((data$Actual[(data$Date>=test)&(data$Date<=test_end)] - 
            data$M13[(data$Date>=test)&(data$Date<=test_end)])/
           data$Actual[(data$Date>=test)&(data$Date<=test_end)])) *100
# 5.05%

####################### XGBoost will be the final model
# Refit the model on all data

data$Loadlag1 = lag(data$Actual,1) 
data$Loadlag2 = lag(data$Actual,2) 
data$Loadlag24 = lag(data$Actual,24) 

train_data = data %>% select('Trend','Month','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
train_data = train_data %>% slice(25:35064)

train_data = dummy_cols(train_data,select_columns = 'DayOfWeek')

train_data = train_data %>% select(!'DayOfWeek')

train_data = dummy_cols(train_data,select_columns = 'Month')

train_data = train_data %>% select(!'Month')

str(train_data)

train_matrix = data.matrix(train_data)

train_label_matrix = data.matrix(data %>% select("Actual") %>% 
                                   slice(25:35064))

predict_data = data %>% select('Trend','Month','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
predict_data = predict_data %>% slice(35065:43848)

predict_data = dummy_cols(predict_data,select_columns = 'DayOfWeek')

predict_data = predict_data %>% select(!'DayOfWeek')

predict_data = dummy_cols(predict_data,select_columns = 'Month')

predict_data = predict_data %>% select(!'Month')

str(predict_data)

predict_data = data.matrix(predict_data)

predict_label_matrix = data.matrix(data %>% select("Actual")) %>% 
                                     slice(35065:43848)

M = xgboost(data = train_matrix, label = train_label_matrix,
              nround = 8,
              objective = "reg:squarederror",
              eta = .99,
              num_parallel_tree = 10) 

M_fitted = predict(M, train_matrix)

data$M=NA 

data$M[25:35064] = M_fitted

# predict
i=35065 # First predicting observation number 
data$M[i] = predict(M, data.matrix(predict_data[i-35064,]))

for(i in 35066:43848){
  data$Loadlag1[i] = ifelse(is.na(data$Actual[i-1]),data$M[i-1],data$Actual[i-1]) 
  data$Loadlag2[i] = ifelse(is.na(data$Actual[i-2]),data$M[i-2],data$Actual[i-2])
  data$Loadlag24[i] = ifelse(is.na(data$Actual[i-24]),data$M[i-24],data$Actual[i-24])
  predict_data = data %>% select('Trend','Month','DayOfWeek',"Hour","Temperature","Loadlag1","Loadlag2","Loadlag24")
  predict_data = predict_data %>% slice(35065:43848)
  predict_data = dummy_cols(predict_data,select_columns = 'DayOfWeek')
  predict_data = predict_data %>% select(!'DayOfWeek')
  predict_data = dummy_cols(predict_data,select_columns = 'Month')
  predict_data = predict_data %>% select(!'Month')
  data$M[i] = predict(M, data.matrix(predict_data[i-35064,]))
}

# plot data and overplay predictions (2 months)
data %>% slice(1:(24*7*8)) %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M),col="blue")+
  theme_bw()

data %>%  ggplot(aes(x=Trend,y=Load)) +geom_line() +
  geom_line(aes(x=Trend,y=M),col="blue")+
  theme_bw()

# plot predictions data
data %>% slice(35065:43848) %>%  ggplot(aes(x=Trend,y=Actual)) +geom_line() +
  geom_line(aes(x=Trend,y=M),col="blue")+
  theme_bw()

# Metrics of accuracy

# RMSE training 
sqrt(mean((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M[(data$Date<test)&(data$Trend>=25)])^2))
# 204.4887

# MAPE training 
mean(abs((data$Actual[(data$Date<test)&(data$Trend>=25)] - data$M[(data$Date<test)&(data$Trend>=25)])/
           data$Actual[(data$Date<test)&(data$Trend>=25)])) *100
# 1.30%

# Output fitted and predicted values

data$Predicted.Load[25:35064] = M_fitted
data$Predicted.Load[35065:43848] = data$M[35065:43848]

final_DF = data %>% select("Date","Hour","Predicted.Load")
head(final_DF,30)
tail(final_DF)

write.csv(final_DF,"/Users/gaoyi/Desktop/prediction.csv")