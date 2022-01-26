library(tidyverse)
library(forecast)
library(readxl)

# 1 Nike analysis
# import data
profit = "Nike.xlsx" %>% read_xlsx(range = "B22:DJ22", col_types = "numeric", col_names = FALSE) %>% unlist(use.names = FALSE)
dates = "Nike.xlsx" %>% read_xlsx(range = "B11:DJ11", col_types = "text", col_names = FALSE) %>% unlist(use.names = FALSE)

# trans to dataframe
all_data = data.frame(dates, profit)

# create variable trend:
dim(all_data)[1]
all_data$trend = 1:(dim(all_data)[1])

# create variable quarter:
all_data$quarter = factor(all_data %>% select(dates) %>% unlist %>% substr(start = 8,stop = 8) %>% as.numeric())
all_data
all_data %>% str

# Modeling scenario:
# Training set: start = 1994Q1, end = 2018Q4 
# Testing set: start = 2019Q1, end = 2019Q4
# Predicting set: start = 2020Q1, end = 2022Q1

# Split data into pre-covid and post-covid
data = all_data[1:104,]         # pre-covid
covid = all_data[105:113,]      # post-covid

# Plot all data
all_data %>% ggplot(aes(x=trend, y=profit)) + geom_line() +
  geom_point(aes(color=quarter)) + theme_bw()
# Huge profit drop in 2020 FQ4, but seems to restore in 2021

# Plot pre-covid data
data %>% ggplot(aes(x=trend, y=profit)) + geom_line() +
  geom_point(aes(color=quarter)) + theme_bw()
# Upward trend, seasonal component (additive or multiplicative TBD), cyclical, noise

# make a copy of the profit column and call it actual
data$actual = data$profit
data

# last 4 data points (quarters) are testing set. replace them with NAs in column profit
(dim(data)[1]-4+1) : dim(data)[1]
data$profit[(dim(data)[1]-4+1) : dim(data)[1]] = NA
data

# 1.1 Regression
# 1.1.1 linear trend + additive seasonality
M1 = lm(profit ~ trend + quarter, data=data)
M1 %>% summary

data$M1 = NA
data$M1[!is.na(data$profit)]= M1$fitted.values

data %>% ggplot(aes(x=trend,y=profit)) + geom_line(size=1.25) + 
  geom_line(aes(x=trend,y=M1),color="red",size=1.25) + theme_bw()

tsdisplay(M1$residuals,lag.max=60)
# obvious cyclical pattern in residuals, and significant spike in the ACF graph
# systematic patterns can be modeled by including lags
# according to PACF, try including 1st & 5th lag and 1st seasonal lag

# 1.1.2 linear trend + additive seasonality + 2 nonseasonal lags + 1 seasonal lag
# create first non seasonal lag and first seasonal lag
data$profitlag1 = lag(data$profit,1) 
data$profitlag4 = lag(data$profit,4)
data$profitlag5 = lag(data$profit,5)
data

M2 = lm(profit ~ trend + quarter + profitlag1 + profitlag4 + profitlag5, data=data)
summary(M2)

tsdisplay(M2$residuals,lag.max=60)
# it seems that there is one more significant nonseasonal lag 3

# 1.1.3 linear trend + additive seasonality + 3 nonseasonal lags + 1 seasonal lag
data$profitlag3 = lag(data$profit,3) 

M3 = lm(profit ~ trend + quarter + profitlag1 + profitlag4 + profitlag5 + profitlag3, data=data)
summary(M3)

tsdisplay(M3$residuals,lag.max=60)
# still has one significant spike, but looks better than M2

# carry out the Box test of independence
Box.test(M3$residuals)
# since p-value >0.05, fail to reject Ho and conclude that there is 
# no statistically significant evidence that data are not independent.

# M3 will be the final regression model w/ additive seasonality

# fit on train data
data$M3=NA 
data$M3residuals = NA

data$M3[!is.na(data$profit) &!is.na(data$profitlag1) & !is.na(data$profitlag3) &
          !is.na(data$profitlag4) & !is.na(data$profitlag5)] = M3$fitted.values

data$M3residuals[!is.na(data$profit) &!is.na(data$profitlag1) & !is.na(data$profitlag3) &
                   !is.na(data$profitlag4) & !is.na(data$profitlag5)] = M3$residuals

head(data)
tail(data)

# predict on test data
i=101 # First testing observation number 
data$M3[i] = predict(M3,newdata=data[i,])

for(i in 102:(dim(data)[1])){
  data$profitlag1[i] = ifelse(is.na(data$profit[i-1]),data$M3[i-1],data$profit[i-1]) 
  data$profitlag3[i] = ifelse(is.na(data$profit[i-3]),data$M3[i-3],data$profit[i-3])
  data$profitlag4[i] = ifelse(is.na(data$profit[i-4]),data$M3[i-4],data$profit[i-4])
  data$profitlag5[i] = ifelse(is.na(data$profit[i-5]),data$M3[i-5],data$profit[i-5])
  data$M3[i] = predict(M3,newdata=data[i,])
}

# plot everything
data$TrainTest = "Test"
data$TrainTest[1:(dim(data)[1]-4)] = "Train"
data %>% tail

data %>% ggplot(aes(x=trend,y=actual)) + 
  geom_line(size=1.25) + geom_line(aes(x=trend,y=M3,color=TrainTest),size=1.25) + 
  theme_bw()

data %>% filter(trend >= 101) %>% 
  ggplot(aes(x=trend,y=actual)) + geom_line(aes(color="Testing period"),size=1.25) + 
  geom_line(aes(x=trend,y=M3,color="Forecast on the testing set"),size=1.25) + 
  scale_colour_manual("",values=c("Testing period" = "black","Forecast on the testing set" = "red")) + 
  theme_bw()

# Evaluation
M3.Training.RMSE = data %>% filter(trend < 101) %>% {sqrt(mean((.$actual - .$M3)^2,na.rm = TRUE))} 
M3.Training.RMSE

M3.Testing.RMSE = data %>% filter(trend >= 101) %>% {sqrt(mean((.$actual - .$M3)^2,na.rm = TRUE))}
M3.Testing.RMSE

M3.Training.MAPE = data %>% filter(trend < 101) %>% {mean(abs((.$actual -.$M3)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M3.Training.MAPE

M3.Testing.MAPE = data %>% filter(trend >= 101) %>% {mean(abs((.$actual -.$M3)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M3.Testing.MAPE

# 1.1.4 linear trend + multiplicative seasonality
data$logprofit =log(data$profit)
data %>% ggplot(aes(x=trend,y=logprofit)) + geom_line() + theme_bw()

M4 = lm(logprofit ~ trend + quarter, data=data)
M4 %>% summary

tsdisplay(M4$residuals,lag.max=60)
# obvious cyclical pattern in residuals, and significant spike in the ACF graph
# systematic patterns can be modeled by including lags
# Try including lags 1,3,5

# 1.1.5 linear trend + multiplicative seasonality + 3 nonseasonal lags
data$logprofitlag1 = lag(data$logprofit,1) 
data$logprofitlag3 = lag(data$logprofit,3) 
data$logprofitlag5 = lag(data$logprofit,5) 

M5 = lm(logprofit ~ trend + quarter + logprofitlag1  + logprofitlag3 + logprofitlag5, data=data)
summary(M5)

tsdisplay(M5$residuals,lag.max=60)
# lag 4 also seems to be significant

# 1.1.6 linear trend + multiplicative seasonality + 3 nonseasonal lags + 1 seasonal lag
data$logprofitlag4 = lag(data$logprofit,4) 

M6 = lm(logprofit ~ trend + quarter + logprofitlag1  + logprofitlag3 + logprofitlag4 + logprofitlag5, data=data)
summary(M6)

tsdisplay(M6$residuals,lag.max=60)
# still has significant spike but looks better than M5

# carry out the Box test of independence
Box.test(M6$residuals)
# since p-value >0.05, fail to reject Ho and conclude that there is 
# no statistically significant evidence that data are not independent.

# M6 will be the final regression model w/ multiplicative seasonality

# fit on train data
data$M6log=NA 
data$M6logresiduals = NA

data$M6log[!is.na(data$logprofit) & !is.na(data$logprofitlag1) & !is.na(data$logprofitlag3) &
             !is.na(data$logprofitlag4) & !is.na(data$logprofitlag5)] = M6$fitted.values

data$M6logresiduals[!is.na(data$logprofit) & !is.na(data$logprofitlag1) & !is.na(data$logprofitlag3) &
                      !is.na(data$logprofitlag4) & !is.na(data$logprofitlag5)] = M6$residuals

head(data)
tail(data)

# predict on test data
i=101 # First testing observation number 
data$M6log[i] = predict(M6,newdata=data[i,])

for(i in 102:(dim(data)[1])){
  data$logprofitlag1[i] = ifelse(is.na(data$logprofit[i-1]),data$M6log[i-1],data$logprofit[i-1]) 
  data$logprofitlag3[i] = ifelse(is.na(data$logprofit[i-3]),data$M6log[i-3],data$logprofit[i-3])
  data$logprofitlag4[i] = ifelse(is.na(data$logprofit[i-4]),data$M6log[i-4],data$logprofit[i-4])
  data$logprofitlag5[i] = ifelse(is.na(data$logprofit[i-5]),data$M6log[i-5],data$logprofit[i-5])
  data$M6log[i] = predict(M6,newdata=data[i,])
}

# undo the log transformation
data$M6 = NA 
data$M6residuals = NA

data$M6 =exp(data$M6log)
data$M6residuals =exp(data$M6logresiduals)

# plot everything
data %>% ggplot(aes(x=trend,y=actual)) + 
  geom_line(size=1.25) + geom_line(aes(x=trend,y=M6,color=TrainTest),size=1.25) + 
  theme_bw()

data %>% filter(trend >= 101) %>% 
  ggplot(aes(x=trend,y=actual)) + geom_line(aes(color="Testing period"),size=1.25) + 
  geom_line(aes(x=trend,y=M6,color="Forecast on the testing set"),size=1.25) + 
  scale_colour_manual("",values=c("Testing period" = "black","Forecast on the testing set" = "red")) + 
  theme_bw()
# test data seems to be less accurately predicted than in the 
# additive seasonality case

# Evaluation
M6.Training.RMSE = data %>% filter(trend < 101) %>% {sqrt(mean((.$actual - .$M6)^2,na.rm = TRUE))} 
M6.Training.RMSE

M6.Testing.RMSE = data %>% filter(trend >= 101) %>% {sqrt(mean((.$actual - .$M6)^2,na.rm = TRUE))}
M6.Testing.RMSE

M6.Training.MAPE = data %>% filter(trend < 101) %>% {mean(abs((.$actual -.$M6)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M6.Training.MAPE

M6.Testing.MAPE = data %>% filter(trend >= 101) %>% {mean(abs((.$actual -.$M6)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M6.Testing.MAPE

# 1.2 ARIMA
y = ts(data$actual,start=c(1994,1),frequency = 4)

y.train = window(y,end=c(2018,4))
y.test = window(y,start=c(2019,1))
y.train
y.test

M = auto.arima(y.train,lambda="auto")
M

MF = forecast(M,h=length(y.test),level=95) 
MF

autoplot(y,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + theme_bw()

autoplot(y.test,size=1.25) + autolayer(MF$mean,size=1.25) + theme_bw()

accuracy(MF,y.test)[,c("RMSE","MAPE")]

############### Summary of regressions and ARIMA ###############
y.test.M3.forecast = ts(data$M3[(dim(data)[1]-4+1) : dim(data)[1]], start=c(2019,1),frequency=4)
y.test.M6.forecast = ts(data$M6[(dim(data)[1]-4+1) : dim(data)[1]], start=c(2019,1),frequency=4)

autoplot(y.test, size = 2) + autolayer(MF$mean,series="ARIMA",size = 2) + geom_point(size = 5) +
  autolayer(y.test.M3.forecast,series="M3", size = 2) + 
  autolayer(y.test.M6.forecast,series="M6", size = 2) + 
  theme_bw()

AccuracyMetrics = data.frame(Model=c("M3", "M6", "ARIMA"),
                             TrainingRMSE=round(c(M3.Training.RMSE, M6.Training.RMSE, accuracy(MF,y.test)[1,"RMSE"]),2),
                             TestingRMSE=round(c(M3.Testing.RMSE, M6.Testing.RMSE, accuracy(MF,y.test)[2,"RMSE"]),2),
                             TrainingMAPE=c(M3.Training.MAPE, M6.Training.MAPE, paste0(round(accuracy(MF,y.test)[1,"MAPE"],2),"%")),
                             TestingMAPE=c(M3.Testing.MAPE, M6.Testing.MAPE, paste0(round(accuracy(MF,y.test)[2,"MAPE"],2),"%")))
AccuracyMetrics

# M3 performs the best using both testing RMSE and MAPE.
# M3 is the champion model.

# Now retrain M3 using all pre-covid data
data = all_data

# make a copy of the profit column and call it actual
data$actual = data$profit

# last 9 data points (quarters) are predicting set. replace them with NAs in column profit
data$profit[(dim(data)[1]-9+1) : dim(data)[1]] = NA
data

data$profitlag1 = lag(data$profit,1) 
data$profitlag3 = lag(data$profit,3)
data$profitlag4 = lag(data$profit,4)
data$profitlag5 = lag(data$profit,5)
data

M = lm(profit ~ trend + quarter + profitlag1 + profitlag4 + profitlag5 + profitlag3, data=data)
summary(M)

# fit on train data
data$M = NA 
data$Mresiduals = NA

data$M[!is.na(data$profit) &! is.na(data$profitlag1) & !is.na(data$profitlag3) &
         !is.na(data$profitlag4) & !is.na(data$profitlag5)]= M$fitted.values

head(data)
tail(data,15)

# predict on predicting data
i=105
data$M[i] = predict(M, newdata=data[i,])

for(i in 106:(dim(data)[1])){
  data$profitlag1[i] = ifelse(is.na(data$profit[i-1]),data$M[i-1],data$profit[i-1]) 
  data$profitlag3[i] = ifelse(is.na(data$profit[i-3]),data$M[i-3],data$profit[i-3])
  data$profitlag4[i] = ifelse(is.na(data$profit[i-4]),data$M[i-4],data$profit[i-4])
  data$profitlag5[i] = ifelse(is.na(data$profit[i-5]),data$M[i-5],data$profit[i-5])
  data$M[i] = predict(M,newdata=data[i,])
}

# plot
data$covid = "postcovid"
data$covid[1:(dim(data)[1]-9)] = "precovid"
tail(data,15)

data %>% ggplot(aes(x=trend,y=actual)) + 
  geom_line(size=1.25) + geom_line(aes(x=trend,y=M,color=covid),size=1.25) + 
  theme_bw()
# profit dropped dramatically at the beginning of covid, but later bounced back to a high level

data %>% filter(trend >= 105) %>% 
  ggplot(aes(x=trend,y=actual)) + geom_line(aes(color="Actual"),size=1.25) + 
  geom_line(aes(x=trend,y=M,color="If No Covid"),size=1.25) + 
  scale_colour_manual("",values=c("Actual" = "pink","If No Covid" = "green")) + 
  theme_bw()

# The impact of covid:
data %>% filter(trend >= 105) %>% mutate(impact = actual - M) %>% 
  summarise(sum(impact),mean(impact))

# Conclusion: covid has reduced Nike's profit by $732,640 in total, from Q1 2020 to Q1 2022.
# However, since Nike is restoring fast, the loss is expected to be made up in the coming years.


# 2 Adidas analysis
profit = "Adidas.xlsx" %>% read_xlsx(range = "V22:DG22", col_types = "numeric", col_names = FALSE) %>% unlist(use.names = FALSE)
dates = "Adidas.xlsx" %>% read_xlsx(range = "V11:DG11", col_types = "text", col_names = FALSE) %>% unlist(use.names = FALSE)

# trans to dataframe
all_data = data.frame(dates, profit)

# create variable trend:
dim(all_data)[1]
all_data$trend = 1:(dim(all_data)[1])

# create variable quarter:
all_data$quarter = factor(all_data %>% select(dates) %>% unlist %>% substr(start = 8,stop = 8) %>% as.numeric())
all_data
all_data %>% str

# Modeling scenario:
# Training set: start = 1999Q1, end = 2018Q4 
# Testing set: start = 2019Q1, end = 2019Q4
# Predicting set: start = 2020Q1, end = 2021Q2

# Split data into pre-covid and post-covid
data = all_data[1:84,]         # pre-covid
covid = all_data[85:90,]       # post-covid

# Plot all data
all_data %>% ggplot(aes(x=trend, y=profit)) + geom_line() +
  geom_point(aes(color=quarter)) + theme_bw()
# Huge profit drop starting 2020 Q1, worst in Q2, but seems to restore in Q3

# Plot pre-covid data
data %>% ggplot(aes(x=trend, y=profit)) + geom_line() +
  geom_point(aes(color=quarter)) + theme_bw()
# Upward trend, seasonal component (additive), cyclical, noise

# make a copy of the profit column and call it actual
data$actual = data$profit
data

# last 4 data points (quarters) are testing set. replace them with NAs in column profit
(dim(data)[1]-4+1) : dim(data)[1]
data$profit[(dim(data)[1]-4+1) : dim(data)[1]] = NA
data

# 2.1 Regression
# 2.1.1 linear trend + additive seasonality
M1 = lm(profit ~ trend + quarter, data=data)
M1 %>% summary

data$M1 = NA
data$M1[!is.na(data$profit)]= M1$fitted.values

data %>% ggplot(aes(x=trend,y=profit)) + geom_line(size=1.25) + 
  geom_line(aes(x=trend,y=M1),color="red",size=1.25) + theme_bw()

tsdisplay(M1$residuals,lag.max=60)
# obvious cyclical pattern in residuals, and significant spike in the ACF graph
# systematic patterns can be modeled by including lags
# according to PACF, first try including 1st, 3rd, 5th lag

# 2.1.2 linear trend + additive seasonality + 3 nonseasonal lags
# create first non seasonal lags
data$profitlag1 = lag(data$profit,1) 
data$profitlag3 = lag(data$profit,3) 
data$profitlag5 = lag(data$profit,5) 

data

M2 = lm(profit ~ trend + quarter + profitlag1 + profitlag3 + profitlag5, data=data)
summary(M2)

tsdisplay(M2$residuals,lag.max=60)
# it seems that the first seasonal lag is also significant

# 2.1.3 linear trend + additive seasonality + 3 nonseasonal lags + 1 seasonal lag
data$profitlag4 = lag(data$profit,4) 

M3 = lm(profit ~ trend + quarter + profitlag1 + profitlag3 + profitlag4 + profitlag5, data=data)
summary(M3)

tsdisplay(M3$residuals,lag.max=60)
# Now there is no significant spike in ACF or PACF

# carry out the Box test of independence
Box.test(M3$residuals)
# since p-value >0.05, fail to reject Ho and conclude that there is 
# no statistically significant evidence that data are not independent.

# M3 will be the final regression model

# fit on train data
data$M3=NA 
data$M3residuals = NA

data$M3[!is.na(data$profit) &!is.na(data$profitlag1) & !is.na(data$profitlag3) &
          !is.na(data$profitlag4) & !is.na(data$profitlag5)] = M3$fitted.values

data$M3residuals[!is.na(data$profit) &!is.na(data$profitlag1) & !is.na(data$profitlag3) &
                   !is.na(data$profitlag4) & !is.na(data$profitlag5)] = M3$residuals

head(data)
tail(data)

# predict on test data
i=81 # First testing observation number 
data$M3[i] = predict(M3,newdata=data[i,])

for(i in 82:(dim(data)[1])){
  data$profitlag1[i] = ifelse(is.na(data$profit[i-1]),data$M3[i-1],data$profit[i-1]) 
  data$profitlag3[i] = ifelse(is.na(data$profit[i-3]),data$M3[i-3],data$profit[i-3])
  data$profitlag4[i] = ifelse(is.na(data$profit[i-4]),data$M3[i-4],data$profit[i-4])
  data$profitlag5[i] = ifelse(is.na(data$profit[i-5]),data$M3[i-5],data$profit[i-5])
  data$M3[i] = predict(M3,newdata=data[i,])
}

# plot everything
data$TrainTest = "Test"
data$TrainTest[1:(dim(data)[1]-4)] = "Train"
data %>% tail

data %>% ggplot(aes(x=trend,y=actual)) + 
  geom_line(size=1.25) + geom_line(aes(x=trend,y=M3,color=TrainTest),size=1.25) + 
  theme_bw()

data %>% filter(trend >= 81) %>% 
  ggplot(aes(x=trend,y=actual)) + geom_line(aes(color="Testing period"),size=1.25) + 
  geom_line(aes(x=trend,y=M3,color="Forecast on the testing set"),size=1.25) + 
  scale_colour_manual("",values=c("Testing period" = "black","Forecast on the testing set" = "red")) + 
  theme_bw()

# Evaluation
M3.Training.RMSE = data %>% filter(trend < 81) %>% {sqrt(mean((.$actual - .$M3)^2,na.rm = TRUE))} 
M3.Training.RMSE

M3.Testing.RMSE = data %>% filter(trend >= 81) %>% {sqrt(mean((.$actual - .$M3)^2,na.rm = TRUE))}
M3.Testing.RMSE

M3.Training.MAPE = data %>% filter(trend < 81) %>% {mean(abs((.$actual -.$M3)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M3.Training.MAPE

M3.Testing.MAPE = data %>% filter(trend >= 81) %>% {mean(abs((.$actual -.$M3)/.$actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")
M3.Testing.MAPE

# 2.2 ARIMA
y = ts(data$actual,start=c(1999,1),frequency = 4)

y.train = window(y,end=c(2018,4))
y.test = window(y,start=c(2019,1))
y.train
y.test

M = auto.arima(y.train,lambda="auto")
M

MF = forecast(M,h=length(y.test),level=95) 
MF

autoplot(y,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF$mean, series=" Forecast on the \n testing set",size=1.25) + theme_bw()

autoplot(y.test,size=1.25) + autolayer(MF$mean,size=1.25) + theme_bw()
# looks better than regression

accuracy(MF,y.test)[,c("RMSE","MAPE")]

############### Summary of regressions and ARIMA ###############
y.test.M3.forecast = ts(data$M3[(dim(data)[1]-4+1) : dim(data)[1]], start=c(2019,1),frequency=4)

autoplot(y.test, size = 2) + autolayer(MF$mean,series="ARIMA",size = 2) +
  autolayer(y.test.M3.forecast,series="M3", size = 2) + 
  theme_bw()

AccuracyMetrics = data.frame(Model=c("M3", "ARIMA"),
                             TrainingRMSE=round(c(M3.Training.RMSE, accuracy(MF,y.test)[1,"RMSE"]),2),
                             TestingRMSE=round(c(M3.Testing.RMSE, accuracy(MF,y.test)[2,"RMSE"]),2),
                             TrainingMAPE=c(M3.Training.MAPE, paste0(round(accuracy(MF,y.test)[1,"MAPE"],2),"%")),
                             TestingMAPE=c(M3.Testing.MAPE, paste0(round(accuracy(MF,y.test)[2,"MAPE"],2),"%")))
AccuracyMetrics

# ARIMA performs better using both testing RMSE and MAPE.
# ARIMA is the champion model.

# Now retrain ARIMA using all pre-covid data

y = ts(all_data$profit,start=c(1999,1),frequency = 4)

y.train = window(y,end=c(2019,4))
y.predict = window(y,start=c(2020,1))
y.train
y.predict

M = auto.arima(y.train,lambda="auto")
M

MF = forecast(M,h=length(y.predict),level=95) 
MF

autoplot(y,size=1.25) + autolayer(MF$fitted, series="Fitted values",size=1.25) + 
  autolayer(MF$mean, series=" Forecast on the \n predicting set",size=1.25) + theme_bw()

autoplot(y.predict,size=1.25) + autolayer(MF$mean,size=1.25) + theme_bw()
# profit dropped dramatically at the beginning of covid
# Although profit is restoring, it has not recovered to precovid level

# The impact of covid:
comparison = data.frame(actual = y.predict, expected = MF$mean)

comparison %>% mutate(impact = actual - expected) %>% 
  summarise(sum(impact),mean(impact))

# Conclusion: covid has reduced Adidas's profit by $4,580,552 in total, from Q1 2020 to Q2 2021.
# Loss much larger than Nike, and unlike Nike, profit has not recovered to what was expected without covid.