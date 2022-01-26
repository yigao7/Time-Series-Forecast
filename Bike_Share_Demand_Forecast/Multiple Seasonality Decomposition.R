library(rio)
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


########################### EDA ########################### 
summary(data)
############################ prediction ###########
## merage loucation data 
location = read.csv("metro-bike-share-stations-2021-10-01.csv")
head(location)
# add column name for location 
colnames(location)[1] <- "station_id"
colnames(location)[4] <- "locations"
colnames(location)[3] <- "date"
colnames(location)[5] <- "status"
colnames(location)[2] <- "street"
# combine two data set 
data$locations = location$locations[match(data$start_station_id,location$station_id)]
data$status = location$status[match(data$start_station_id,location$station_id)]
head(data)
# select only active status 
data = data[data$status == "Active", ]
head(data)
## convert time to hour 
data$time = as.character(data$start_time)
data$time = as.POSIXct(data$time,format="%Y-%m-%d%k")
head((data))
#remove night hour since which is to prevent large amount of 0 demands. 
data$hour = hour(data$time)
head(data)
data = data[data$hour >= 7 & data$hour <= 22, ]
tail(data)
head(data)
################## DTLA Arima #################################
########## DTLA prediction 
data_DTLA = data[data$locations == "DTLA", ]
#group by 
df_dtla = data_DTLA %>% group_by(time) %>% summarise(trip_id = n())
df_dtla

df_dtla$Day = wday(df_dtla$time,label=T,abbr=F)
df_dtla$Week = week(df_dtla$time)
df_dtla$Month = month(df_dtla$time,label=T,abbr=F)
df_dtla$Year = year(df_dtla$time)

head(df_dtla)

# SPECIFY Training set end data:
y = 2021
m =06
d = 30

# determine length of training set:
n_train = dim(df_dtla[year(df_dtla$time)<=y & month(df_dtla$time)<= m & day(df_dtla$time)<= d,])[1]
n_train

# SPECIFY Testing set start and end dates

# start 
y1 = 2021
m1 =07
d1 = 01

# End
y2 = 2021
m2 =09
d2 = 30

# determine length of testing set:
n_test = dim(df_dtla[year(df_dtla$time)<=y2 & year(df_dtla$time)>=y1 &
                    month(df_dtla$time)<=m2 & month(df_dtla$time)>=m1 &
                    day(df_dtla$time)<=d2 & day(df_dtla$time)>=d1,])[1]
n_test

# create a multiple seasonality time series:

y=msts(na.omit(df_dtla$trip_id)[1:(n_train+n_test)],seasonal.periods = c(24,7*24,365.25*24))

length(window(y,end=c(1,365.25*23)))
length(head(y,n_train))

M = mstl(head(y,n_train)) 
autoplot(M)

MF = forecast(M,method="arima",h=n_test,level=95)
accuracy(MF,tail(y,n_test))

#############################################################
###navies 
M2 = ets(y.train)
MF2 = forecast(M,h = length(y.test),level=95)

MF2$ fitted
MF2$mean
accuracy(MF2,y.test)




################end ################3
# Scenario 1 train before 2021 Q3, test = 2021 Q3 
# divide data into training and testing
# replace values for testing data set 

#df_dtla$Actual = df_dtla$trip_id
#df_dtla$trip_id[df_dtla$time>="2021-07-01"] = NA

#tail(df_dtla)

#df_dtla$trend = df_dtla$time
####################3
################## westside Arima #################################
########## westside prediction 
data_west = data[data$locations == "Westside", ]
#group by 
df_w = data_west %>% group_by(time) %>% summarise(trip_id = n())
df_w

df_w$Day = wday(df_w$time,label=T,abbr=F)
df_w$Week = week(df_w$time)
df_w$Month = month(df_w$time,label=T,abbr=F)
df_w$Year = year(df_w$time)

head(df_w)

# SPECIFY Training set end data:
y = 2021
m =06
d = 30

# determine length of training set:
n_train = dim(df_w[year(df_w$time)<=y & month(df_w$time)<= m & day(df_w$time)<= d,])[1]
n_train

# SPECIFY Testing set start and end dates

# start 
y1 = 2021
m1 =07
d1 = 01

# End
y2 = 2021
m2 =09
d2 = 30

# determine length of testing set:
n_test = dim(df_w[year(df_w$time)<=y2 & year(df_w$time)>=y1 &
                       month(df_w$time)<=m2 & month(df_w$time)>=m1 &
                       day(df_w$time)<=d2 & day(df_w$time)>=d1,])[1]
n_test

# create a multiple seasonality time series:


y_w=msts(na.omit(df_w$trip_id)[1:(n_train+n_test)],seasonal.periods = c(24,7*24,365.25*24))

length(window(y_w,end=c(1,365.25*23)))
length(head(y_w,n_train))

M2 = mstl(head(y_w,n_train)) 
autoplot(M2)

MF = forecast(M2,method="arima",h=n_test,level=95)
accuracy(MF,tail(y_w,n_test))

##### joinly ###############################################
df_all = data%>% group_by(time) %>% summarise(trip_id = n())
df_all


df_all$Day = wday(df_all$time,label=T,abbr=F)
df_all$Week = week(df_all$time)
df_all$Month = month(df_all$time,label=T,abbr=F)
df_all$Year = year(df_all$time)

head(df_all)

# SPECIFY Training set end data:
y = 2021
m =06
d = 30

# determine length of training set:
n_train = dim(df_all[year(df_all$time)<=y & month(df_all$time)<= m & day(df_all$time)<= d,])[1]
n_train

# SPECIFY Testing set start and end dates

# start 
y1 = 2021
m1 =07
d1 = 01

# End
y2 = 2021
m2 =09
d2 = 30

# determine length of testing set:
n_test = dim(df_all[year(df_all$time)<=y2 & year(df_all$time)>=y1 &
                    month(df_all$time)<=m2 & month(df_all$time)>=m1 &
                    day(df_all$time)<=d2 & day(df_all$time)>=d1,])[1]
n_test

# create a multiple seasonality time series:


y_all=msts(df_all$trip_id[1:(n_train+n_test)],seasonal.periods = c(24,7*24))

length(window(y_all,end=c(1,365.25*23)))
length(head(y_all,n_train))

M3 = mstl(head(y_all,n_train)) 
autoplot(M3)

MF = forecast(M3,method="arima",h=n_test,level=95)
accuracy(MF,tail(y_all,n_test))

