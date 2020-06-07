#title-bike rent analysis
#clear environment removing object
rm(list=ls())
# woetrking directory
getwd()
#Load Libraries
library(rpart)                          #regression trees
library(tidyverse)                          #data manipulation
library(ggthemes)                        #visualization themes
library(ggrepel)
library(randomForest)                   #random forest
library(corrgram)                       #correlation plotting

#load dataset
df=read.csv("bike-rent-data.csv")
#Exploratory data analysis
###   understand data
#we have 731 observation and 16 variables
#understand structure of dataset
str(df)
#exploratory data analysis
#remove instant column as it is showing indeing only
df <- select(df,-instant) 

#first of all we have to rename variable to the readable name
#rename variables to readable name

names(df)[1] = "date"
names(df)[3] = "year"
names(df)[4] = "month"
names(df)[8] = "weather_condition"
names(df)[9] = "temperature"
names(df)[10] = "feeling_temperature"
names(df)[11] = "humidity"
names(df)[15] = "total_count"

#change datatype of respective variable to required format
df$date=as.Date(df$date)
df$season=as.factor(df$season)
df$year=as.factor(df$year)
df$month=as.factor(df$month)
df$holiday=as.factor(df$holiday)
df$weekday=as.factor(df$weekday)
df$workingday=as.factor(df$workingday)
df$weather_condition=as.factor(df$weather_condition)

#check column names and data types
str(df)
###     missing value analysis

#missing_val = sapply(df,function(x) sum(is.na(x)))
#print(missing_val)

##using mice library
library(mice)
md.pattern(df,rotate.names = TRUE)
#no missing value found

### outlier analysis
numeric_index = sapply(df,is.numeric) #selecting only numeric

#generate new data with only numerical data
numeric_data = df[,numeric_index]

#save column names of numeric_data to cnames
cnames = colnames(numeric_data)

## remove outlier using boxplot method
# # #loop to remove from all variables
for(i in cnames){
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val))
  df = df[which(!df[,i] %in% val),]
}


### visualization and trend of distribution
#see distribution with season
ggplot(df,aes(season,total_count))+
  ggeom_point(s(color=weather_condition))
#we can see, there is low count in season 1
#one another  thing we can observe that is low count in bad weather

#effect of temp on daily basis
ggplot(df,aes(temp,total_count))+
  geom_point(aes(color=humidity,size=windspeed))

#we can clearlyobserve that count is proportional to the temp
#but whenever is high wind speed ,count is less(big size dot)
#dark dots show low humidity that shows lower

##effect of holiday in  a month
ggplot(df,aes(month,total_count))+
  geom_boxplot(aes(color=holiday))
#we can compare median of holiday and working day, there is slightly difference only.
##################################Feature Selection################################################
## Correlation Plot 
corrgram(df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#removing atemp as it is highly correlated with temp
#removing casual and registered because this is what we want to predict
#removing holiday,workingday and weekdays as they do not have contribution

## Dimension Reduction
df = subset(df, select = -c(date,atemp,weekday,holiday,workingday,casual,registered))
#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(df), 0.7 * nrow(df))
train = df[train_index,]
test = df[-train_index,]
# ##rpart for regression
fit = rpart(total_count ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-9])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,9], predictions_DT)

#Error Rate: 26.22
#Accuracy: 73.78

#run regression model
lm_model = lm(total_count ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,-9])

#Calculate MAPE
MAPE(test[,9], predictions_LR)

#Error Rate: 19.46
#acuracy: 80.54

### run random forest model
model_RF = randomForest(total_count~.,train, ntree=1000)

#predict
predictions_RF = predict(model_RF, test[,-9])

#MAPE
MAPE(test[,9],predictions_RF)
#error= 21.53
#accuracy = 78.47

##run svr model
library(e1071)
model_SVR= svm(total_count~.,train, type="eps-regression", kernel="radial")

#predict
prediction_svr = predict(model_SVR, test[-9])

#MAPE
MAPE(test[,9],prediction_svr)
#error= 19.28%
#accuracy = 80.72%

##freeze the linear regression model as it is has minimal error