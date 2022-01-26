library(rvest)
library(dygraphs)
library(forecast)
library(tidyverse)

# define variable that stores url of the website that contains the data table

url='https://www.transtats.bts.gov/Data_Elements.aspx?Data=1'

#1.
data = url %>% read_html() %>% html_nodes(xpath = '//*[@id="GridView1"]') %>% 
  html_table 
# []=vector
#[[]]=list

data = data [[1]]
data

#2.
data$DOMESTIC = data %>% select(DOMESTIC) %>% unlist %>% str_replace_all(',', '')
data$INTERNATIONAL= data %>% select(INTERNATIONAL) %>% unlist %>% str_replace_all(',', '')
data$DOMESTIC=as.numeric(data$DOMESTIC)
data$INTERNATIONAL=as.numeric(data$INTERNATIONAL)

data

#3.
# remove rows of 'Month = TOTAL'
#data$Month=as.numeric(data$Month)
#data=na.omit(data)
data
data = data[data$Month != 'TOTAL',]
data

# Create a trend
dim(data)
data$Trend = 1:(dim(data)[1])
data

data %>% ggplot(aes(x=Trend, y=DOMESTIC)) + geom_line() +theme_bw() 
data %>% ggplot(aes(x=Trend, y=INTERNATIONAL)) + geom_line() +theme_bw() 
# For DOMESTIC, there is a slight increasing from 2002 to 2019, and a drastic decrease in 2020.
# There are some seasonality components and noises.  
# For INTERNATIONAL, we observe there is an upward trend from 2002 to 2019 with a seasonality. However, there is
# a drastic decrease in 2020.

#4
data %>% ggplot(aes(x=Trend, y=DOMESTIC)) + geom_line() + geom_point(aes(color=Month)) +theme_bw() 
data %>% ggplot(aes(x=Trend, y=INTERNATIONAL)) + geom_line() + geom_point(aes(color=Month)) +theme_bw() 

# 5
# divide data by 1 mln
data$DOMESTIC=data$DOMESTIC/1000000
data$INTERNATIONAL=data$INTERNATIONAL/1000000

y = ts(data[,c("DOMESTIC",'INTERNATIONAL')], start=c(2002,10), frequency = 12)
type = cbind(y)
dygraph(type) %>% dyAxis("y", label = "DOMESTIC, in mln passengers") %>%
  dyAxis("y2", label = "INTERNATIONAL, in mln passengers", independentTicks = TRUE) %>%
  dySeries("INTERNATIONAL", axis = 'y2') %>% dyRangeSelector()


# 6 & 7

#######################################################################################################################
#######################################################################################################################
# Regression Approach
#######################################################################################################################
#######################################################################################################################

data
domestic= data %>% select(Year, Month, DOMESTIC, Trend)
international= data %>% select(Year, Month, INTERNATIONAL, Trend)
domestic
international

domestic %>%  ggplot(aes(x=Trend,y=DOMESTIC)) + geom_line() + theme_bw()

# Scenario 1
# domestic
domestic$Actual = domestic$DOMESTIC
data1=domestic %>% filter(Year <= 2008)

# replace last 12 values in DOMESTIC column with NAs

data1$DOMESTIC[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(DOMESTIC ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$DOMESTIC)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=DOMESTIC)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=64 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 65:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[64,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[64,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2008) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 1.307

Regression.Testing.RMSE = data1 %>% filter(Year>2007 & (Year<= 2008)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 5.274

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2008) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 2%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2007 & (Year<=2008 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 8.8%


# Model 2
data1$DOMESTICLag1 = lag(data1$DOMESTIC,1)
data1$DOMESTICLag2 = lag(data1$DOMESTIC,2)
M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 

#I am going to store fitted/predicted values on training set + forecast on testing sets in column M and residuals in column Mresiduals. 

# Crate columns M and Mresiduas and fill with NAs
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
          !is.na(data1$DOMESTICLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
                   !is.na(data1$DOMESTICLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()


# Since lags are included we need to create a loop to calculate predictions:


data1
dim(data1)

i=64 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])


for(i in 65:(dim(data1)[1])){
  data1$DOMESTICLag1[i] = ifelse(is.na(data1$DOMESTIC[i-1]),data1$M[i-1],data1$DOMESTIC[i-1])
  data1$DOMESTICLag2[i] = ifelse(is.na(data1$DOMESTIC[i-2]),data1$M[i-2],data1$DOMESTIC[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[64,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[64,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2008) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.929

Regression.Testing.RMSE = data1 %>% filter(Year>2007 & (Year<= 2008)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 3.66

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2008) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 1.3%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2007 & (Year<=2008 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 5.39%

################
# international
international$Actual = international$INTERNATIONAL
data1=international %>% filter(Year <= 2008)

data1$INTERNATIONAL[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(INTERNATIONAL ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$INTERNATIONAL)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=INTERNATIONAL)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=64 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 65:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

predict(M,newdata=data1[64,],interval = "prediction") 
predict(M,newdata=data1[64,],interval = "confidence")

# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2008) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.413

Regression.Testing.RMSE = data1 %>% filter(Year>2007 & (Year<= 2008)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 0.993

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2008) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 2.87%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2007 & (Year<=2008 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 6.36%


# Model 2
data1$INTERNATIONALLag1 = lag(data1$INTERNATIONAL,1)
data1$INTERNATIONALLag2 = lag(data1$INTERNATIONAL,2)
M = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
          !is.na(data1$INTERNATIONALLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
                   !is.na(data1$INTERNATIONALLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=64 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 65:(dim(data1)[1])){
  data1$INTERNATIONALLag1[i] = ifelse(is.na(data1$INTERNATIONAL[i-1]),data1$M[i-1],data1$INTERNATIONAL[i-1])
  data1$INTERNATIONALLag2[i] = ifelse(is.na(data1$INTERNATIONAL[i-2]),data1$M[i-2],data1$INTERNATIONAL[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}
predict(M,newdata=data1[64,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[64,],interval = "confidence")

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2008) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.259

Regression.Testing.RMSE = data1 %>% filter(Year>2007 & (Year<= 2008)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 1.010

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2008) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 1.67%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2007 & (Year<=2008 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 6.34%
#####################################################
#####################################################  
# Scenario 2
# domestic
domestic$Actual = domestic$DOMESTIC
data1=domestic %>% filter(Year <= 2019)

# replace last 12 values in DOMESTIC column with NAs

data1$DOMESTIC[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(DOMESTIC ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$DOMESTIC)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=DOMESTIC)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=196 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 197:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[196,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[196,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2019) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 2.79

Regression.Testing.RMSE = data1 %>% filter(Year>2018 & (Year<= 2019)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 6.85

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2019) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 4.55%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2018 & (Year<=2019 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 9.69%

# Model 2
data1$DOMESTICLag1 = lag(data1$DOMESTIC,1)
data1$DOMESTICLag2 = lag(data1$DOMESTIC,2)
M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 

#I am going to store fitted/predicted values on training set + forecast on testing sets in column M and residuals in column Mresiduals. 

# Crate columns M and Mresiduas and fill with NAs
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
          !is.na(data1$DOMESTICLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
                   !is.na(data1$DOMESTICLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()


# Since lags are included we need to create a loop to calculate predictions:


data1
dim(data1)

i=196 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])


for(i in 197:(dim(data1)[1])){
  data1$DOMESTICLag1[i] = ifelse(is.na(data1$DOMESTIC[i-1]),data1$M[i-1],data1$DOMESTIC[i-1])
  data1$DOMESTICLag2[i] = ifelse(is.na(data1$DOMESTIC[i-2]),data1$M[i-2],data1$DOMESTIC[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[196,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[196,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2019) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 1.047

Regression.Testing.RMSE = data1 %>% filter(Year>2018 & (Year<= 2019)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 2.94

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2019) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 1.53%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2018 & (Year<=2019 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 3.89%

################
# international
international$Actual = international$INTERNATIONAL
data1=international %>% filter(Year <= 2019)

data1$INTERNATIONAL[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(INTERNATIONAL ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$INTERNATIONAL)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=INTERNATIONAL)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=196 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 197:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

predict(M,newdata=data1[196,],interval = "prediction") 
predict(M,newdata=data1[196,],interval = "confidence")

# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2019) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.838

Regression.Testing.RMSE = data1 %>% filter(Year>2018 &(Year<= 2019)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 1.37

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2019) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 5.27%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2018& (Year<=2019 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 5.4%


# Model 2
data1$INTERNATIONALLag1 = lag(data1$INTERNATIONAL,1)
data1$INTERNATIONALLag2 = lag(data1$INTERNATIONAL,2)
M = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
          !is.na(data1$INTERNATIONALLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
                   !is.na(data1$INTERNATIONALLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=196 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 197:(dim(data1)[1])){
  data1$INTERNATIONALLag1[i] = ifelse(is.na(data1$INTERNATIONAL[i-1]),data1$M[i-1],data1$INTERNATIONAL[i-1])
  data1$INTERNATIONALLag2[i] = ifelse(is.na(data1$INTERNATIONAL[i-2]),data1$M[i-2],data1$INTERNATIONAL[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}
predict(M,newdata=data1[196,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[196,],interval = "confidence")

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2019) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.428

Regression.Testing.RMSE = data1 %>% filter(Year>2018 & (Year<= 2019)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 0.899

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2019) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 2.39%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2018 & (Year<=2019 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 3.52%


#####################################################
#####################################################  
# Scenario 3
# domestic
domestic$Actual = domestic$DOMESTIC
data1=domestic %>% filter(Year <= 2020)

# replace last 12 values in DOMESTIC column with NAs

data1$DOMESTIC[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(DOMESTIC ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$DOMESTIC)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=DOMESTIC)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=208# First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 209:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[208,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[208,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2020) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 3.08

Regression.Testing.RMSE = data1 %>% filter(Year>2019& (Year<= 2020)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 40.70

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2020) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE # 4.89%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2019 & (Year<=2020 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 348.95%

# Model 2
data1$DOMESTICLag1 = lag(data1$DOMESTIC,1)
data1$DOMESTICLag2 = lag(data1$DOMESTIC,2)
M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 

#I am going to store fitted/predicted values on training set + forecast on testing sets in column M and residuals in column Mresiduals. 

# Crate columns M and Mresiduas and fill with NAs
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
          !is.na(data1$DOMESTICLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
                   !is.na(data1$DOMESTICLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()


# Since lags are included we need to create a loop to calculate predictions:


data1
dim(data1)

i=208 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])


for(i in 209:(dim(data1)[1])){
  data1$DOMESTICLag1[i] = ifelse(is.na(data1$DOMESTIC[i-1]),data1$M[i-1],data1$DOMESTIC[i-1])
  data1$DOMESTICLag2[i] = ifelse(is.na(data1$DOMESTIC[i-2]),data1$M[i-2],data1$DOMESTIC[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}

# you can also include an argument in the predict function to include the prediction or confidence interval. You can store them in the data table in columns.  For example
predict(M,newdata=data1[208,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[208,],interval = "confidence")


# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2020) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 1.09

Regression.Testing.RMSE = data1 %>% filter(Year>2019 & (Year<= 2020)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 46.90

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2020) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE #1.56%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2019 & (Year<=2020 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 395.32%

################
# international
international$Actual = international$INTERNATIONAL
data1=international %>% filter(Year <= 2020)

data1$INTERNATIONAL[(dim(data1)[1]-12+1) : dim(data1)[1]] = NA
data1 %>%  head

data1 %>% tail(14)

# Model 1
M = lm(INTERNATIONAL ~ Trend + Month,data=data1)
summary(M)

data1$M = NA 
data1$M[!is.na(data1$INTERNATIONAL)]= M$fitted.values

data1 %>% ggplot(aes(x=Trend,y=INTERNATIONAL)) + geom_line(size=1.25) + 
  geom_line(aes(x=Trend,y=M),color="red",size=1.25) + theme_bw()

tsdisplay(M$residuals,lag.max=60)
# There are several significant lags in the ACF graphs.

data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) ] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=208 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 209:(dim(data1)[1])){
  data1$M[i] =  predict(M,newdata=data1[i,])
}

predict(M,newdata=data1[208,],interval = "prediction") 
predict(M,newdata=data1[208,],interval = "confidence")

# Create a column that helps color training and testing  differently

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2020) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.87

Regression.Testing.RMSE = data1 %>% filter(Year>2019 & (Year<= 2020)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 16.07

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2020) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE #5.44%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2019 & (Year<=2020 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 1387.43%


# Model 2
data1$INTERNATIONALLag1 = lag(data1$INTERNATIONAL,1)
data1$INTERNATIONALLag2 = lag(data1$INTERNATIONAL,2)
M = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2,data=data1)
tsdisplay(M$residuals,lag.max=60)
# It seems that there are no significant lags.


#We can carry out the Box test of independence:
Box.test(M$residuals)
# Since p-value >0.05, we fail to reject Ho and conclude that there is no
# statistically significant evidence that data are not independent. 
data1$M=NA
data1$Mresiduals = NA

data1$M[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
          !is.na(data1$INTERNATIONALLag2)]=M$fitted.values

data1$Mresiduals[!is.na(data1$INTERNATIONAL) &!is.na(data1$INTERNATIONALLag1) & 
                   !is.na(data1$INTERNATIONALLag2)] =M$residuals

# Residual plots
data1%>% ggplot(aes(x=Trend, y=Mresiduals)) + geom_line(color="green")+
  geom_hline(yintercept=0)+ theme_bw()

data1
dim(data1)

i=208 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])

for(i in 209:(dim(data1)[1])){
  data1$INTERNATIONALLag1[i] = ifelse(is.na(data1$INTERNATIONAL[i-1]),data1$M[i-1],data1$INTERNATIONAL[i-1])
  data1$INTERNATIONALLag2[i] = ifelse(is.na(data1$INTERNATIONAL[i-2]),data1$M[i-2],data1$INTERNATIONAL[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,])
}
predict(M,newdata=data1[208,],interval = "prediction") # lwr = lower bound, upr = upper bound
predict(M,newdata=data1[208,],interval = "confidence")

data1$TrainTest = "Test"
1:(dim(data1)[1]-12)
data1$TrainTest[1:(dim(data1)[1]-12)] = "Train"
data1$TrainTest

# Plot data and overlay fitted/predicted values on the training set and the forecast on the testing set. Note that since we used 2 seasonal lags, we do not have fitted/predicted values for the first 2 seasonal cycles (2 years)

data1 %>% ggplot(aes(x=Trend,y=Actual)) + geom_line(size=1.25) +
  geom_line(aes(x=Trend,y=M,color=TrainTest),size=1.25) +
  theme_bw()

# Evaluate accuracy
# RMSE
Regression.Training.RMSE = data1 %>% filter(Year<2020) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}
Regression.Training.RMSE # 0.44

Regression.Testing.RMSE = data1 %>% filter(Year>2019 & (Year<= 2020)) %>% 
  {sqrt(mean((.$Actual - .$M)^2,na.rm = TRUE))}

Regression.Testing.RMSE # 16.397

# MAPE on the training set
Regression.Training.MAPE = data1 %>% filter(Year<2020) %>% 
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Training.MAPE #2.45%

# MAPE on the testing set
Regression.Testing.MAPE = data1 %>% filter(Year>2019 & (Year<=2020 )) %>%  
  {mean(abs((.$Actual -.$M)/.$Actual),na.rm = TRUE)*100} %>% round(2) %>% paste0("%")

Regression.Testing.MAPE # 1416.34%


#######################################################################################################################
#######################################################################################################################
# ARIMA Approach
#######################################################################################################################
#######################################################################################################################
y=ts(data[,c("DOMESTIC",'INTERNATIONAL')], start=c(2002,10), frequency = 12)
domestic=y[,"DOMESTIC"]
international=y[,"INTERNATIONAL"]

# Scenario 1
# domestic
y.train_domestic = window(domestic,start=c(2002,10),end=c(2007,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_domestic = window(domestic,start=c(2008,1),end=c(2008,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_domestic
y.test_domestic

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_domestic,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_domestic),level=95)
MF

Acf(y.train_domestic, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_domestic, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()


# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_domestic)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.38%
# Arima.Testing MAPE  5.1%
# Arima.Training RMSE  1.044
# Arima.Testing RMSE 3.29

# international
y.train_international = window(international,start=c(2002,10),end=c(2007,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_international = window(international,start=c(2008,1),end=c(2008,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_international
y.test_international

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_international,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_international),level=95)
MF

Acf(y.train_international, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_international, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()

# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_international)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.88%
# Arima.Testing MAPE  5.72%
# Arima.Training RMSE  0.319
# Arima.Testing RMSE 0.856

##############################
# Scenario 2
# domestic
y.train_domestic = window(domestic,start=c(2002,10),end=c(2018,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_domestic = window(domestic,start=c(2019,1),end=c(2019,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_domestic
y.test_domestic

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_domestic,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_domestic),level=95)
MF

Acf(y.train_domestic, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_domestic, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()


# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_domestic)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.44%
# Arima.Testing MAPE  1.16%
# Arima.Training RMSE  1.034
# Arima.Testing RMSE 1.12

# international
y.train_international = window(international,start=c(2002,10),end=c(2018,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_international = window(international,start=c(2019,1),end=c(2019,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_international
y.test_international

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_international,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_international),level=95)
MF

Acf(y.train_international, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_international, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()

# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_international)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.79%
# Arima.Testing MAPE  1.62%
# Arima.Training RMSE  0.33
# Arima.Testing RMSE 0.384

#####################################
# Scenario 3
# domestic
y.train_domestic = window(domestic,start=c(2002,10),end=c(2019,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_domestic = window(domestic,start=c(2020,1),end=c(2020,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_domestic
y.test_domestic

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_domestic,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_domestic),level=95)
MF

Acf(y.train_domestic, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_domestic, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()


# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_domestic)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.44%
# Arima.Testing MAPE  401%
# Arima.Training RMSE  1.05
# Arima.Testing RMSE 48.18

# international
y.train_international = window(international,start=c(2002,10),end=c(2019,12)) # or you can skip start: y.train = window(y,end=c(2007,6))
y.test_international = window(international,start=c(2020,1),end=c(2020,12)) # or you can skip end: y.train = window(y,start=c(2007,7))

y.train_international
y.test_international

# Build an auto.arima model using training set. Include lambda="auto" to transform the data to stabilize variation (transform multiplicative seasonality to additive - if lambda="auto" is included - R will transform and then undo transformation for you automatically and you will not need to do it manually like we did in regression: log and then exp)

M = auto.arima(y.train_international,lambda="auto")
M  
# Generate forecast on the testing set. h = forecast horizon, i.e. how many future values we want to forecast = length of the testing set =12. Let's include 95% prediction intervals.

MF = forecast(M,h=length(y.test_international),level=95)
MF

Acf(y.train_international, lag.max=60) # There are several significant lags

# Visualize data and overlay fitted values and testing set:
autoplot(MF) + autolayer(M$fitted, series="fitted") +
  autolayer(y.test_international, series="testing \n data")

# Residual plot
autoplot(M$residuals) + geom_hline(yintercept=0)+ theme_bw()

# Accuracy metrics on both training and testing set for this scenario:
accuracy(MF,y.test_international)[,c("RMSE","MAPE")]
# Arima.Training MAPE  1.723%
# Arima.Testing MAPE  1479.18%
# Arima.Training RMSE  0.329
# Arima.Testing RMSE 17.18



# Build a table
domestic_summary = data.frame(matrix(ncol = 15, nrow =3))
colnames(domestic_summary) <- c('Model', 'Scenario1_RMSE_training', 'Scenario1_RMSE_testing','Scenario1_MAPE_training', 'Scenario1_MAPE_testing',
                                'Scenario2_RMSE_training', 'Scenario2_RMSE_testing','Scenario2_MAPE_training', 'Scenario2_MAPE_testing',
                                'Scenario3_RMSE_training', 'Scenario3_RMSE_testing','Scenario3_MAPE_training', 'Scenario3_MAPE_testing',
                                "Average_MAPE_training", "Average_MAPE_testing")
domestic_summary[1,]=c("Regression (trend+month)",1.31,5.27,2,8.8,2.79,6.85,4.55,9.69,3.08,40.7,4.89,348.95,3.81,122.48)
domestic_summary[2,]=c("Regression (trend+month+lag1+lag2)",0.93,3.66,1.3,5.39,1.05,2.94,1.53,3.89,1.09,46.9,1.56,395.32,1.46,134.87)
domestic_summary[3,]=c("auto.arima",1.044,3.29,1.38,5.1,1.034,1.12,1.44,1.16,1.05,48.18,1.44,401.97,1.42,136.07)
domestic_summary


international_summary = data.frame(matrix(ncol = 15, nrow =3))
colnames(international_summary) <- c('Model', 'Scenario1_RMSE_training', 'Scenario1_RMSE_testing','Scenario1_MAPE_training', 'Scenario1_MAPE_testing',
                                     'Scenario2_RMSE_training', 'Scenario2_RMSE_testing','Scenario2_MAPE_training', 'Scenario2_MAPE_testing',
                                     'Scenario3_RMSE_training', 'Scenario3_RMSE_testing','Scenario3_MAPE_training', 'Scenario3_MAPE_testing',
                                     "Average_MAPE_training", "Average_MAPE_testing")
international_summary[1,]=c("Regression (trend+month)",0.413,0.993,2.87,6.36,0.838,1.37,5.27,5.4,0.87,16.07,5.44,1387.43,4.53,466.4)
international_summary[2,]=c("Regression (trend+month+lag1+lag2)",0.259,1.01,1.67,6.34,0.428,0.899,2.39,3.52,0.44,16.40,2.45,1416.34,2.17,475.4)
international_summary[3,]=c("auto.arima",0.319,0.856,1.88,5.72,0.33,0.384,1.79,1.62,0.329,17.18,1.723,1479.18,1.798,495.5)
international_summary

# 8
# By comparing the MAPEs, even though the basic model has the lowest average MAPE for both domestic and international, we still select
# the regression models with two lags are the champion models. When look at the Acf graphs,  the basic models for both domestic and international have several 
# significant spikes. Compared to auto arima, regression with two lags have smaller average MAPEs for both domestic and international. 
data
domestic= data %>% select(Year, Month, DOMESTIC, Trend)
international= data %>% select(Year, Month, INTERNATIONAL, Trend)

# domestic
domestic$Actual = domestic$DOMESTIC
data1=domestic
data1$DOMESTICLag1 = lag(data1$DOMESTIC,1)
data1$DOMESTICLag2 = lag(data1$DOMESTIC,2)
M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2,data=data1)

data1 %>% tail


data1[nrow(data1) + 1,] = c(2021,list("8"),NA, 227, NA, data1[226,"DOMESTIC"], data1[225,"DOMESTIC"])
data1[nrow(data1) + 1,] = c(2021,list("9"),NA,228, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2021,list("10"),NA,229, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2021,list("11"),NA,230, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2021,list("12"),NA,231, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("1"),NA,232, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("2"),NA,233, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("3"),NA,234, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("4"),NA,235, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("5"),NA,236, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("6"),NA,237, NA, NA, NA)
data1[nrow(data1) + 1,] = c(2022,list("7"),NA,238, NA, NA, NA)


data1 %>% tail
dim(data1)
data1$M=NA
data1$Mresiduals = NA
data1$M[!is.na(data1$DOMESTIC) &!is.na(data1$DOMESTICLag1) & 
          !is.na(data1$DOMESTICLag2) ]=M$fitted.values
data1$Mresiduals[!is.na(data1$DOMESTIC)&!is.na(data1$DOMESTICLag1) & 
                   !is.na(data1$DOMESTICLag2)] =M$residuals
data1$lwr = NA
data1$upr = NA

i=227 # First testing observation number
data1$M[i] =  predict(M,newdata=data1[i,])[1]
data1$lwr[i] =  predict(M,newdata=data1[i,], interval='prediction')[2]
data1$upr[i] =  predict(M,newdata=data1[i,], interval='prediction')[3]

for(i in 228:238){
  data1$DOMESTICLag1[i] = ifelse(is.na(data1$DOMESTIC[i-1]),data1$M[i-1],data1$DOMESTIC[i-1])
  data1$DOMESTICLag2[i] = ifelse(is.na(data1$DOMESTIC[i-2]),data1$M[i-2],data1$DOMESTIC[i-2])
  data1$M[i] =  predict(M,newdata=data1[i,], interval='prediction')[1]
  data1$lwr[i] =  predict(M,newdata=data1[i,], interval='prediction')[2]
  data1$upr[i] =  predict(M,newdata=data1[i,], interval='prediction')[3]
}

df = NULL
for(i in 227:238){
  df = rbind(df, data.frame(data1[i,c("Year", "Month")]))
}
df2= NULL
for(i in 227:238){
  df2 = rbind(df2, data.frame(predict(M,newdata=data1[i,],interval = "prediction")))
}
table_domestic <-cbind(df, df2)
table_domestic

# international
data
international= data %>% select(Year, Month, INTERNATIONAL, Trend)
international$Actual = international$INTERNATIONAL
data2=international
data2$INTERNATIONALLag1 = lag(data2$INTERNATIONAL,1)
data2$INTERNATIONALLag2 = lag(data2$INTERNATIONAL,2)
M = lm(INTERNATIONAL ~ Trend + Month +INTERNATIONALLag1 + INTERNATIONALLag2,data=data2)

data2 %>% tail

data2[nrow(data2) + 1,] = c(2021,list("8"),NA, 227, NA, data2[226,"INTERNATIONAL"], data2[225,"INTERNATIONAL"])
data2[nrow(data2) + 1,] = c(2021,list("9"),NA,228, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2021,list("10"),NA,229, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2021,list("11"),NA,230, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2021,list("12"),NA,231, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("1"),NA,232, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("2"),NA,233, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("3"),NA,234, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("4"),NA,235, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("5"),NA,236, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("6"),NA,237, NA, NA, NA)
data2[nrow(data2) + 1,] = c(2022,list("7"),NA,238, NA, NA, NA)

dim(data2)
data2$M=NA
data2$Mresiduals = NA
data2$M[!is.na(data2$INTERNATIONAL) &!is.na(data2$INTERNATIONALLag1) & 
          !is.na(data2$INTERNATIONALLag2) ]=M$fitted.values
data2$Mresiduals[!is.na(data2$INTERNATIONAL)&!is.na(data2$INTERNATIONALLag1) & 
                   !is.na(data2$INTERNATIONALLag2)] =M$residuals
data2$lwr = NA
data2$upr = NA

i=227 # First testing observation number
data2$M[i] =  predict(M,newdata=data2[i,])[1]
data2$lwr[i] =  predict(M,newdata=data2[i,], interval='prediction')[2]
data2$upr[i] =  predict(M,newdata=data2[i,], interval='prediction')[3]

for(i in 228:238){
  data2$INTERNATIONALLag1[i] = ifelse(is.na(data2$INTERNATIONAL[i-1]),data2$M[i-1],data2$INTERNATIONAL[i-1])
  data2$INTERNATIONALLag2[i] = ifelse(is.na(data2$INTERNATIONAL[i-2]),data2$M[i-2],data2$INTERNATIONAL[i-2])
  data2$M[i] =  predict(M,newdata=data2[i,], interval='prediction')[1]
  data2$lwr[i] =  predict(M,newdata=data2[i,], interval='prediction')[2]
  data2$upr[i] =  predict(M,newdata=data2[i,], interval='prediction')[3]
}

df = NULL
for(i in 227:238){
  df = rbind(df, data.frame(data2[i,c("Year", "Month")]))
}

df2= NULL
for(i in 227:238){
  df2 = rbind(df2, data.frame(predict(M,newdata=data2[i,],interval = "prediction")))
}
table_international <-cbind(df, df2)
table_international


# 9
data1 %>% tail
data1 %>% ggplot(aes(x=Trend,y=DOMESTIC))+geom_line()+geom_point()+
  geom_line(aes(x=Trend, y=M), color = "red") + 
  geom_line(data=data1[1:226,], aes(x=Trend,y=DOMESTIC), color = "blue") + 
  geom_point(data=data1[227:238,], aes(x=Trend,y=DOMESTIC), color = "blue", size=2)

data2 %>% tail
data2 %>% ggplot(aes(x=Trend,y=INTERNATIONAL))+geom_line()+geom_point()+
  geom_line(aes(x=Trend, y=M), color = "red") + 
  geom_line(data=data2[1:226,], aes(x=Trend,y=INTERNATIONAL), color = "blue") + 
  geom_point(data=data2[227:238,], aes(x=Trend,y=INTERNATIONAL), color = "blue", size=2)

# For champion model (Q7):  plot all historical data, future forecasts with prediction intervals, 
# and fitted values. Save your graph in a pdf or jpeg format and submit it on blackboard. 
###################### Revised:
data1 %>% ggplot(aes(x=Trend,y=DOMESTIC))+geom_line() +
  geom_line(data=data1[1:226,], aes(x=Trend, y=M,color = "fitted")) + 
  geom_line(data=data1[227:238,], aes(x=Trend,y=M, color = "predicted")) + 
  geom_line(data=data1[227:238,], aes(x=Trend,y=lwr, color = "lwr & upr")) + 
  geom_line(data=data1[227:238,], aes(x=Trend,y=upr, color = "lwr & upr")) +
  scale_colour_manual("",values=c("fitted" = "light blue","predicted" = "red", "lwr & upr" = "pink")) + 
  theme_bw()

data2 %>% ggplot(aes(x=Trend,y=INTERNATIONAL))+geom_line() +
  geom_line(data=data2[1:226,], aes(x=Trend, y=M,color = "fitted")) + 
  geom_line(data=data2[227:238,], aes(x=Trend,y=M, color = "predicted")) + 
  geom_line(data=data2[227:238,], aes(x=Trend,y=lwr, color = "lwr & upr")) + 
  geom_line(data=data2[227:238,], aes(x=Trend,y=upr, color = "lwr & upr")) +
  scale_colour_manual("",values=c("fitted" = "light blue","predicted" = "red", "lwr & upr" = "pink")) + 
  theme_bw()

#10
# By plot with forecast with prediction intervals, our model are fitted in this data set. 
# But we should exclusive the some big event happening in training data set.
# In long term, those external big event effect cannot representing the regular pattern, 
# It is better if we can consider external effect as outliers.  


#11
# For example, the Covid-19 is the external huge effect for airline demand,
# which might casued miss under or over prediction. 
# so when we build model we need to include more data, so we have more power to forecasting our demand. 

#12
# Covid-19 impact for domestic:  = data1[1:226]
y.test_domestic = window(domestic,start=c(2020,1),end=c(2020,12))

y.test_domestic

M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2,data=data1) 

MF = forecast(M,h=length(y.test_domestic),level=95)
MF$mean

comparison = sum(y.test_domestic - MF$mean )
comparison
paste0(round(100 * sum(y.test_domestic-MF$mean) / sum(y.test_domestic),2),"%")
# In conclusion, the covid-19 impact for DOMESTICS there are and 25.66% higher than predictions. 
# based on the data, the passenger traffic has not recovered yet.

# Covid-19 impact for international 
international= data %>% select(Year, Month, INTERNATIONAL, Trend)
international$Actual = international$INTERNATIONAL
data2=international
data2$INTERNATIONALLag1 = lag(data2$INTERNATIONAL,1)
data2$INTERNATIONALLag2 = lag(data2$INTERNATIONAL,2)
M2 = lm(INTERNATIONAL ~ Trend + Month +INTERNATIONALLag1 + INTERNATIONALLag2,data=data2)

MF = forecast(M,h=length(y.test_international),level=95)
MF
MF$mean

comparison = sum(y.test_international - MF$mean )
comparison
paste0(round(100 * sum(y.test_international-MF$mean) / sum(y.test_international),2),"%")
# In conclusion, the Covid-19 impact for international: there are and 298.18 % demand lower than prediction. 
# based on the data, the passenger traffic has not recovered yet.