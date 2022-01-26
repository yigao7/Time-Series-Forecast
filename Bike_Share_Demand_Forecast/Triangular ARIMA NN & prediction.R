library(rio)
library(forecast)
library(lubridate)
library(ggplot2)
library(dplyr)

url=c("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/MetroBikeShare_2016_Q3_trips-2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/09/metro-bike-share-trips-2016-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/la_metro_gbfs_trips_Q1_2017-2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/07/la_metro_gbfs_trips_Q2_2017.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/metro-bike-share-trips-2017-q3.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/02/metro-bike-share-trips-2017-q4-v2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/04/metro-bike-share-trips-2018-q1.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/08/metro-bike-share-trips-2018-q2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/10/metro-bike-share-trips-2018-q3.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/metro-bike-share-trips-2018-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/04/metro-bike-share-trips-2019-q1.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/metro-bike-share-trips-2019-q2.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/10/metro-bike-share-trips-2019-q3-1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/metro-bike-share-trips-2019-q4.csv.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/04/metro-bike-share-trips-2020-q1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/03/metro-trips-2020-q2-v2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/10/metro-trips-2020-q3.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/01/metro-trips-2020-q4.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/04/metro-trips-2021-q1-1.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/metro-trips-2021-q2.zip",
      "https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip"
)

# 2016 Q3
head(import(url[1]),2)
# 2016 Q4
head(import(url[2]),2)
# 2017 Q1
head(import(url[3]),2)
# 2017 Q2
head(import(url[4]),2)
# 2017 Q3
head(import(url[5]),2)
# 2017 Q4
head(import(url[6]),2)
# 2018 Q1
head(import(url[7]),2)
# 2018 Q2
head(import(url[8]),2)
# 2018 Q3
head(import(url[9]),2)
# 2018 Q4
head(import(url[10]),2)
# 2019 Q1
head(import(url[11]),2)
# 2019 Q2
head(import(url[12]),2)
# 2019 Q3
head(import(url[13]),2)
# 2019 Q4
head(import(url[14]),2)
# 2020 Q1
head(import(url[15]),2)
# 2020 Q2
head(import(url[16]),2)
# 2020 Q3
head(import(url[17]),2)
# 2020 Q4
head(import(url[18]),2)
# 2021 Q1
head(import(url[19]),2)
# 2021 Q2
head(import(url[20], which=2),2)
# 2021 Q3
head(import(url[21], which=2),2)
# station
head(import(url[21], which=1),2)

#loop
data = import(url[1])
#data$bike_type=NA
str(data)

data$start_time = mdy_hm(data$start_time)
data$end_time = mdy_hm(data$end_time)
str(data)

FirstQuarterNames = names(data)

for (i in 2:(length(url))){
  print(i)
  if (i<20){
    quarter_i=import(url[i])}
  else{  quarter_i=import(url[i],which=2)}
  colnames(quarter_i)=FirstQuarterNames
  if(i %in% c(2,4,6,7,8,9,10,11,12,14)){
    quarter_i$start_time=ymd_hms(quarter_i$start_time)
    quarter_i$end_time=ymd_hms(quarter_i$end_time)
  }else{
    quarter_i$start_time=mdy_hm(quarter_i$start_time)
    quarter_i$end_time=mdy_hm(quarter_i$end_time)
  }
  common_cols <- intersect(colnames(quarter_i), FirstQuarterNames)
  data=rbind(data[common_cols ], quarter_i[common_cols ])
}

dim(data)
str(data)

head(data,2)
tail(data,2)

station = import(url[21], which=1)
colnames(station)[which(names(station) == "V1")] <- "start_station_id"
station %>% head(2)

# construct full hours dataframe
hour = c(as.POSIXct('2016-07-07 04:00:00',format="%Y-%m-%d%k"))
full_hours = data.frame(hour)
for (i in 2:45879){
  # print(i)
  if(full_hours[i-1,] == as.POSIXct("2017-03-12 01:00:00 PST",format="%Y-%m-%d%k")){
    full_hours[i,] = as.POSIXct("2017-03-12 03:00:00 PST",format="%Y-%m-%d%k")}
  else{
    if(full_hours[i-1,] == as.POSIXct("2018-03-11 01:00:00 PST",format="%Y-%m-%d%k")){
      full_hours[i,] = as.POSIXct("2018-03-11 03:00:00 PST",format="%Y-%m-%d%k")}
    else{
      if(full_hours[i-1,] == as.POSIXct("2019-03-10 01:00:00 PST",format="%Y-%m-%d%k")){
        full_hours[i,] = as.POSIXct("2019-03-10 03:00:00 PST",format="%Y-%m-%d%k")}
      else{
        if(full_hours[i-1,] == as.POSIXct("2020-03-08 01:00:00 PST",format="%Y-%m-%d%k")){
          full_hours[i,] = as.POSIXct("2020-03-08 03:00:00 PST",format="%Y-%m-%d%k")}
        else{
          if(full_hours[i-1,] == as.POSIXct("2021-03-14 01:00:00 PST",format="%Y-%m-%d%k")){
            full_hours[i,] = as.POSIXct("2021-03-14 03:00:00 PST",format="%Y-%m-%d%k")}
          else{
            full_hours[i,] = full_hours[i-1,] + hours(1)}
        }}}}}
head(full_hours)
tail(full_hours)

################ 1. predict demand for whole LA area
### Generate the y's
data$hour = as.character(data$start_time)
data$hour = as.POSIXct(data$hour,format="%Y-%m-%d%k")

data1 = data[,c(1,15)]

demand = data1 %>% group_by(hour) %>% count()

# join full hours table with demand table and replace NA's with 0's
demand = merge(x=full_hours,y=demand,by="hour",all.x=TRUE)
demand[is.na(demand)] = 0

# plotting one day's data
demand[1:24,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()

# plotting one week's data
demand[1:24*7,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()

# plotting one month's data
demand[1:24*7*4,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()
tail(demand)

# Model 1: ARIMA
y = msts(demand$n, seasonal.periods=c(24, 24*7, 24*365.25))

# took forever to train all data, so evaluate all models using:
# train: 2020Q4, 2021Q1, 2021Q2
# test: 2021Q3
y.train = msts(y[37167:43695], seasonal.periods=c(24, 24*7, 24*365.25))
y.test = msts(y[43696:45879], seasonal.periods=c(24, 24*7, 24*365.25))
length(y.train)
length(y.test)

for (i in 1:5){
  for (j in 1:8){
    for (k in 1:10){
      print(c(i,j,k))
      M = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(i, j, k)), lambda="auto")
      MF=forecast(M, xreg=fourier(y.train,K=c(i,j,k), h=length(y.test)))
      print(accuracy(msts(MF$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")])
    }
  }
}

M = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(3, 3, 2)), lambda="auto")
M

# generate forecast
MF=forecast(M, xreg=fourier(y.train,K=c(3,3,2), h=length(y.test)))

autoplot(msts(y[37167:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(MF$fitted, series="Fitted values") + 
  autolayer(MF$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 9.480985

# Model 2: Neural Net
# tried different combinations of K with smaller data, and found (5,10,15) a good approximate
M1 = nnetar(y.train, seasonal = F, xreg=fourier(y.train, K = c(5, 10, 15)),MaxNWts=4279)
M1

# generate forecast
MF1=forecast(M1, xreg = fourier(y.train, K = c(5, 10, 15), h=length(y.test)))

autoplot(msts(y[37167:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(MF1$fitted, series="Fitted values") + 
  autolayer(MF1$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF1$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]
# RMSE = 9.968064

# Model 3: Smoothing
M2 = ets(y.train, seasonal = F, xreg=fourier(y.train, K = c(3, 3, 2)))
#  No model able to be fitted

################ 2. predict demand for DTLA
data2 = merge(x=data,y=station,by="start_station_id",all.x=TRUE)
data2 %>% head()
data_DTLA = data2 %>% filter(V4 == 'DTLA')
data_DTLA = data_DTLA[,c(2,15)]

demand_DTLA = data_DTLA %>% group_by(hour) %>% count()

# join full hours table with demand table and replace NA's with 0's
demand_DTLA = merge(x=full_hours,y=demand_DTLA,by="hour",all.x=TRUE)
demand_DTLA[is.na(demand_DTLA)] = 0

# plotting one day's data
demand_DTLA[1:24,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()

# plotting one week's data
demand_DTLA[1:24*7,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()

# plotting one month's data
demand_DTLA[1:24*7*4,] %>% ggplot(aes(x=hour, y=n)) + geom_line() + theme_bw()

# Model 1: ARIMA
y = msts(demand_DTLA$n, seasonal.periods=c(24, 24*7, 24*365.25))

# took forever to train all data, so evaluate all models using:
# train: 2020Q4, 2021Q1, 2021Q2
# test: 2021Q3
y.train = msts(y[37167:43695], seasonal.periods=c(24, 24*7, 24*365.25))
y.test = msts(y[43696:45879], seasonal.periods=c(24, 24*7, 24*365.25))
length(y.train)
length(y.test)

for (i in 1:5){
  for (j in 1:5){
    for (k in 1:5){
      print(c(i,j,k))
      M = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(i, j, k)), lambda="auto")
      MF=forecast(M, xreg=fourier(y.train,K=c(i,j,k), h=length(y.test)))
      print(accuracy(msts(MF$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")])
    }
  }
}

# best k is (2 2 3)

M = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(2, 2, 3)), lambda="auto")
M

# generate forecast
MF=forecast(M, xreg=fourier(y.train,K=c(2,2,3), h=length(y.test)))

autoplot(msts(y[37167:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(MF$fitted, series="Fitted values") + 
  autolayer(MF$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 5.844722

# Model 2: Neural Net

for (i in 1:5){
  for (j in 1:5){
    for (k in 1:5){
      if(k>=j){
        if (j>=i){
          print(c(i,j,k))
          M = nnetar(y.train, seasonal = F, xreg=fourier(y.train, K = c(i,j,k)),MaxNWts=4279)
          MF=forecast(M, xreg=fourier(y.train,K=c(i,j,k), h=length(y.test)))
          print(accuracy(msts(MF$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")])
      }
     }
    }
  }
}
# best k is (1 1 4)

M1 = nnetar(y.train, seasonal = F, xreg=fourier(y.train, K = c(1, 1, 4)),MaxNWts=4279)
M1

# Average of 20 networks, each of which is
# a 48-24-1 network with 1201 weights
# options were - linear output units 

# generate forecast
MF1=forecast(M1, xreg = fourier(y.train, K = c(1, 1, 4), h=length(y.test)))

autoplot(msts(y[37167:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(MF1$fitted, series="Fitted values") + 
  autolayer(MF1$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

autoplot(msts(y[43696:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(msts(MF1$mean,seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

autoplot(msts(y[45855:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(msts(MF1$mean[2160:2184],seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

autoplot(msts(y[45711:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(msts(MF1$mean[2016:2184],seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

autoplot(msts(y[45159:45879], seasonal.periods=c(24, 24*7, 24*365.25))) + 
  autolayer(msts(MF1$mean[1464:2184],seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF1$mean, seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]
# RMSE = 5.510655

# Model 3: Smoothing
M2 = ets(y.train, seasonal = F, xreg=fourier(y.train, K = c(1, 1, 2)))
#  No model able to be fitted

################# Now try to remove night hours
demand_DTLA$h = as.character(demand_DTLA$hour)
demand_DTLA$h = substr(demand_DTLA$h,12,13)
demand_DTLA_daytime = demand_DTLA %>% filter(!h %in% c('23','24','00','01','02','03','04','05','06'))
demand_DTLA_daytime = demand_DTLA_daytime[,c(1,2)]

# Model 1: ARIMA
y = msts(demand_DTLA_daytime$n, seasonal.periods=c(16, 16*7, 16*365.25))

# took forever to train all data, so evaluate all models using:
# train: 2020Q4, 2021Q1, 2021Q2
# test: 2021Q3
y.train = msts(y[24753:29120], seasonal.periods=c(16, 16*7, 16*365.25))
y.test = msts(y[29121:30592], seasonal.periods=c(16, 16*7, 16*365.25))
length(y.train)
length(y.test)

M = auto.arima(y.train, seasonal = F, xreg=fourier(y.train, K = c(2, 2, 3)), lambda="auto")
M

# generate forecast
MF=forecast(M, xreg=fourier(y.train,K=c(2,2,3), h=length(y.test)))

autoplot(msts(y[24753:30592], seasonal.periods=c(16, 16*7, 16*365.25))) + 
  autolayer(MF$fitted, series="Fitted values") + 
  autolayer(MF$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF$mean, seasonal.periods=c(16, 16*7, 16*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]

# RMSE = 6.679373

# Model 2: Neural Net

M1 = nnetar(y.train, seasonal = F, xreg=fourier(y.train, K = c(1, 1, 4)),MaxNWts=4279)
M1
# Average of 20 networks, each of which is
# a 45-23-1 network with 1082 weights
# options were - linear output units 

# generate forecast
MF1=forecast(M1, xreg = fourier(y.train, K = c(1, 1, 4), h=length(y.test)))

autoplot(msts(y[24753:30592], seasonal.periods=c(16, 16*7, 16*365.25))) + 
  autolayer(MF1$fitted, series="Fitted values") + 
  autolayer(MF1$mean, series=" Forecast on the \n testing set") + 
  theme_bw()

autoplot(msts(y[29121:30592], seasonal.periods=c(16, 16*7, 16*365.25))) + 
  autolayer(msts(MF1$mean,seasonal.periods=c(16, 16*7, 16*365.25),start=c(1,1)), series=" Forecast on the \n testing set") + 
  theme_bw()

accuracy(msts(MF1$mean, seasonal.periods=c(16, 16*7, 16*365.25),start=c(1,1)),y.test)[,c("RMSE","MAPE")]
# RMSE = 7.226563


################# Make Final prediction for DTLA
# Use all available data
y = msts(demand_DTLA$n, seasonal.periods=c(24, 24*7, 24*365.25))

M_final = nnetar(y, seasonal = F, xreg=fourier(y, K = c(1, 1, 4)),MaxNWts=4279)
M_final

autoplot(msts(y[8767:45879], seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1))) + 
  autolayer(msts(M_final$fitted[8767:45879],seasonal.periods=c(24, 24*7, 24*365.25),start=c(1,1)), series="Fitted values", color='orange') + 
  theme_bw()

# construct Q4 dataframe
start = c(as.POSIXct('2021-10-01 00:00:00',format="%Y-%m-%d%k"))
Q4 = data.frame(start)
for (i in 2:2208){
        Q4[i,] = Q4[i-1,] + hours(1)
        }
head(Q4)
tail(Q4)

dim(Q4)[1]

# generate forecast
MF_final=forecast(M_final, xreg = fourier(y, K = c(1, 1, 4), h=dim(Q4)[1]))

MF_final

autoplot(MF_final$mean) + theme_bw()

data.frame(MF_final$mean)

?cbind
Q4_predictions = cbind(Q4, data.frame(MF_final$mean))

tail(Q4_predictions,10)

write.csv(Q4_predictions,"/Users/gaoyi/Desktop/prediction.csv")