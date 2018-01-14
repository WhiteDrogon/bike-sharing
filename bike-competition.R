#bike sharing 
setwd("/home/hemanth/Downloads")
train=read.csv("train.csv")
test=read.csv("test.csv")
test$registered =0
test$casual=0
test$count=0
data = rbind(train,test)
#find if there are missing variables in the data
is.na(data)
table(is.na(data))
#understand the data through visualization  
par(mfrow=c(4,2))
par(mar=rep(2,4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)
table(data$weather)
#convert discrete variables into factors
data$season<-as.factor(data$season)
data$weather<-as.factor(data$weather)
data$holiday<-as.factor(data$holiday)
data$workingday<-as.factor(data$workingday)
#test hourley hypothesis
#trying to find out if the bike rentals depends on hourely rates
data$hour <- substr(data$datetime,12,13)
data$hour <- as.factor(data$hour)
train_data <- data[as.integer(substr(data$datetime,9,10))<20,]
test_data <- data[as.integer(substr(data$datetime,9,10))>19,]
par(mfrow=c(1,1))
boxplot(train_data$count~train_data$hour,xlab="hour",ylab="count")
#classifies demand of bike into 3 categories 
#1.high demand - 7-9 , 17-19
#2.medium demand - 10-16 
#3. extremely low demand 20-24,0-6
boxplot(train_data$casual~train_data$hour,xlab="hour",ylab="casual")
boxplot(train_data$registered~train_data$hour,xlab="hour",ylab="registered users")
#plot amount of bikes borrowed per day 
date=substr(train_data$datetime,1,10)
train_data$days<-weekdays(as.Date(date))
str(data)
str(train_data)
boxplot(train_data$count~train_data$days,xlab="days",ylab="count")
#to determine if the number of cycles are dependent on the days of the week
boxplot(train_data$casual~train_data$days,main="causal vs weekdays")
boxplot(train_data$registered~train_data$days,main="registered vs weekdays")
#test if the rain affects the number of people renting the bikes
boxplot(train_data$casual~train_data$weather,main="casual vs weather")
boxplot(train_data$registered~train_data$weather,main="registered vs weather")
#yes in case of rains or heavy rains the number of bikes rented are very less
#to check if it depends on the temparature windspeeed and all that 
sub=data.frame(train_data$registered,train_data$casual,train_data$count,train_data$temp,train_data$humidity,train_data$atemp,train_data$windspeed)
cor(sub)
data$year<-substr(data$datetime,1,4)
train_data$hour <- as.integer(train_data$hour)
test_data$hour <- as.integer(test_data$hour)
install.packages("rattle")
install.packages("rpart.plot")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
d=rpart(registered~hour,data=train_data)
fancyRpartPlot(d)
#we create bins and add extra columns to the table to make our prediction more accurate
data=rbind(train_data,test_data)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==18 | data$hour==19]=7
table(data$dp_reg)
boxplot(data$registered~data$hour,main="registered vs hourly rent")
data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month >3]=2
data$year_part[data$year=='2011' & data$month >6]=3
data$year_part[data$year=='2011' & data$month >9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month >3]=6
data$year_part[data$year=='2012' & data$month >6]=7
data$year_part[data$year=='2012' & data$month >9]=8
table(data$year_part)
data$day_type = ""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$workingday==1]="working day"
data$weekend =0
data$weekend[data$days=="sunday" | data$days=="saturday"]=1
#convert all discrete variables into factors
train_data$hour <- as.factor(train_data$hour)
test_data$hour <- as.factor(test_data$hour)
train_data$month <- substr(train_data$datetime,6,7)
test_data$month <- substr(test_data$datetime,6,7)
train_data$days <-as.factor(train_data$days)
test_data$days <- as.factor(test_data$days)
train_data <- data[as.integer(substr(data$datetime,9,10)) < 20,]
test_data <- data[as.integer(substr(data$datetime,9,10))>19,]
set.seed(415)
library(randomForest)
logreg = log(train_data$registered)
fit1 <- randomForest(registered~hour+workingday+days+holiday+day_type+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,data=train_data,importance = TRUE,nt=250)
prediction <- predict(fit1,test_data)
s <- data.frame(datatime = test_data$datetime,count = test_data$datetime)
write.csv(s,file="solution.csv",row.names = FALSE)
