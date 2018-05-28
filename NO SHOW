library(readr)
noshow <- read_csv("no_show.csv")
names(noshow)

## Renaming of variable noshow
colnames(noshow)[14]=c("Shows")
names(noshow)

######Cleaning data


#Identifiying NA
sapply(noshow,function(y) sum(lengths(which(is.na(y)))))

summary(noshow)
table(noshow$Age)

##Removing outliers
#Removing observations with age -1 and storing it as data
a<-subset(noshow,noshow$Age %in% -1)
data<-noshow[!(row.names(noshow) %in% row.names(a)),]

#Check for duplicates
data = unique(data)
str(data)

#####Characteristics

##scholarship and neighborhood
sort(tapply(data$Scholarship, data$Neighbourhood, mean), decreasing = TRUE) #percentage of Nh with patients receiving scholarships


##Case of a Patient ID 99637671331
sort(table(data$PatientId), decreasing = TRUE) #patient with over 84 appointments and more

table(data$PatientId == 99637671331, data$Shows) #shows of Patient ID 99637671331
table(data$PatientId == 99637671331, data$AppointmentDay) #AppointmentDays Patient ID 99637671331

#closer look on the patient Patient 99637671331 on a day where he had 6 appointments
a<-as.data.frame(subset(data,data$AppointmentDay=="2016-06-01T00:00:00Z"))
table(a$PatientId==99637671331,a$ScheduledDay)
b<-as.data.frame(subset(a,a$PatientId==99637671331))
insights<-as.data.frame(b[,c(1:7,14)])  

##correlation of Age and Hipertension
plot(tapply(data$Hipertension,data$Age,mean), main = "Correlation of Hipertension and Age",
        xlab = "Age",
        ylab = "percentage of patient with Hipertension")

#####Preparing Dataset

## Random selection of 10000 observations from the dataset and storing it as newdata
set.seed(100)
newdata<-data[sample(1:nrow(data),10000,replace=F),]

## Converting Dependent variable "Shows" to binary
inds<-which(newdata$Shows %in% "No")

levels(newdata$Shows)<-c(levels(newdata$Shows), "1","0")
newdata$Shows[inds]<- "1"
newdata$Shows[-inds]<- "0"
newdata$Shows=factor(newdata$Shows)

table(newdata$Shows)

## Separating Schedule time and Schedule day
newdata$ScheduledDay= as.POSIXct(newdata$ScheduledDay,format="%d %b %Y :%H:%M:%OS")

scheduleday<-as.data.frame(substr(newdata$ScheduledDay,1,10))
colnames(scheduleday)[1]=c("ScheduleDay")

scheduletime<-as.data.frame(substr(newdata$ScheduledDay,11,20))
colnames(scheduletime)[1]=c("ScheduleTime")

str(scheduleday) #day of scheduling the appointment


scheduleday <- as.Date(scheduleday$ScheduleDay, tz="UTC")

## Creating new variable "waiting time" by subtracting Appointment day and Schedule day 
waitingtime = difftime(as.POSIXct(newdata$AppointmentDay, tz="UTC"), as.POSIXct(scheduleday), units="days")
table(waitingtime)

## Adding this newly created variabel "Waiting time" to the dataframe and storing it as newdata1
newdata1=as.data.frame(cbind(newdata,waitingtime))  



# check correlation with  new variable waitung time
datacor1 = newdata1					
datacor1$waitingtime <- as.numeric(datacor1$waitingtime)
cor(datacor1[,c(1,2,6,8,9,10,11,12,15)]) 


## newdata1 dataset is used for model implementation


#### Train and Test Split
library(caTools)
set.seed(2000)
split <- sample(1:nrow(newdata1), size=round(0.7*nrow(newdata)), replace=FALSE)

train <- newdata1[split,]  #Only takes rows that are in samp
test <- newdata1[-split,]

### Logistic Regression

## without waiting time
log<-glm(formula=Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log)

## Predictions
logpredictions<-predict(log,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions>0.5)
(5+2407)/nrow(test) ## accuracy 0.804 seed 2000

## with variable "waiting time" 
log1<-glm(formula=Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log1)

## Predictions
logpredictions1<-predict(log1,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions1>0.5)
(9+2395)/nrow(test) ## accuracy 0.801 seed 2000

## Receiver Operator Curve (ROC)
library(ROCR)
ROCRlog<-prediction(logpredictions,test$Shows)
ROCRperf<-performance(ROCRlog,"tpr","fpr")

## Threshold labels
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

## Log AUC
as.numeric(performance(ROCRlog,"auc")@y.values)

#################Decison Tree
require(rpart)
require(rpart.plot)

## without varible "waitingtime"
tree<-rpart(formula = Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
            maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree)

#Predictions
predictions<-predict(tree,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions)
(2412+2)/nrow(test) ## accurarcy 0.804 seed 2000

## with variable "waiting time"
tree1<-rpart(formula = Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
             maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree1)

#Predictions
predictions1<-predict(tree1,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions1)
(2403+4)/nrow(test) #accurarcy 0.802 seed 2000

############### Random Forest
library(randomForest)
## with variable "waiting time"
rf<-randomForest(Shows~waitingtime+Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2414+2)/nrow(test) ## accuracy 0.805 for seed 2000

## without variable "waiting time"
rf<-randomForest(Shows~Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2414+2)/nrow(test) ## accuracy 0.805 for seed 2000

######################################################################################################
################# Changing the seed to 111 and re-running the model to check for model improvements ##
#### Train and Test Split
library(caTools)
set.seed(111)
split <- sample(1:nrow(newdata1), size=round(0.7*nrow(newdata)), replace=FALSE)

train <- newdata1[split,]  #Only takes rows that are in samp
test <- newdata1[-split,]

### Logistic Regression

## without waiting time
log<-glm(formula=Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log)

## Predictions
logpredictions<-predict(log,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions>0.5)
(3+2419)/nrow(test) ## accurarcy 0.807 seed 111

## with variable "waiting time" 
log1<-glm(formula=Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log1)

## Predictions
logpredictions1<-predict(log1,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions1>0.5)
(12+2403)/nrow(test) ## accuracy 0.805 seed 111


## Receiver Operator Curve (ROC)
library(ROCR)
ROCRlog<-prediction(logpredictions,test$Shows)
ROCRperf<-performance(ROCRlog,"tpr","fpr")

## Threshold labels
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

## Log AUC
as.numeric(performance(ROCRlog,"auc")@y.values)

#################Decison Tree
require(rpart)
require(rpart.plot)

## without varible "waitingtime"
tree<-rpart(formula = Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
            maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree)

#Predictions
predictions<-predict(tree,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions)
(2424+0)/nrow(test) ## accurarcy 0.808 seed 111
 

## with variable "waiting time"
tree1<-rpart(formula = Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
             maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree1)

#Predictions
predictions1<-predict(tree1,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions1)
(2426+0)/nrow(test) ## accurarcy 0.808 seed 111
 
############### Random Forest
library(randomForest)
## with variable "waiting time"
rf<-randomForest(Shows~waitingtime+Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2426+1)/nrow(test) ## accuracy 0.809 seed 111 

## without variable "waiting time"
rf<-randomForest(Shows~Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2426+1)/nrow(test) ## accuracy 0.809 seed 111
####################################################################################################

####################################################################################################
####### Changing the seed to 1234 and re-run the models
#### Train and Test Split
library(caTools)
set.seed(1234)
split <- sample(1:nrow(newdata1), size=round(0.7*nrow(newdata)), replace=FALSE)

train <- newdata1[split,]  #Only takes rows that are in samp
test <- newdata1[-split,]

### Logistic Regression

## without waiting time
log<-glm(formula=Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log)

## Predictions
logpredictions<-predict(log,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions>0.5)
(1+2426)/nrow(test) ## accurarcy 0.809 seed 1234

## with variable "waiting time" 
log1<-glm(formula=Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received+ Gender+Diabetes+Handcap+Neighbourhood,data = train, family = binomial)
summary(log1)

## Predictions
logpredictions1<-predict(log1,newdata = test,type = "response")

## Model Accuracy
table(test$Shows,logpredictions1>0.5)
(8+2410)/nrow(test) ## accurarcy 0.806 seed 1234

## Receiver Operator Curve (ROC)
library(ROCR)
ROCRlog<-prediction(logpredictions,test$Shows)
ROCRperf<-performance(ROCRlog,"tpr","fpr")

## Threshold labels
plot(ROCRperf,colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))

## Log AUC
as.numeric(performance(ROCRlog,"auc")@y.values)

#################Decison Tree
require(rpart)
require(rpart.plot)

## without varible "waitingtime"
tree<-rpart(formula = Shows  ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
            maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree)

#Predictions
predictions<-predict(tree,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions)
(2432+0)/nrow(test) ## accurarcy 0.811 seed 1234 

## with variable "waiting time"
tree1<-rpart(formula = Shows  ~ waitingtime + Age +  Alcoholism + Hipertension + Scholarship + SMS_received, data = train, method = "class", 
             maxdepth = 4, minsplit = 2, minbucket = 5, cp = -1)
prp(tree1)

#Predictions
predictions1<-predict(tree1,newdata = test,type = "class")

## Model Accuracy
table(test$Shows,predictions1)
(2432+0)/nrow(test) #accurarcy 0.811 seed 1234 

############### Random Forest
library(randomForest)
## with variable "waiting time"
rf<-randomForest(Shows~waitingtime+Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2431+1)/nrow(test) ## accuracy 0.811 for seed 1234

## without variable "waiting time"
rf<-randomForest(Shows~Age +  Alcoholism + Hipertension + Scholarship + SMS_received,data = train)

## Predictions
predrf<-predict(rf,newdata = test)

## Accuracay
table(test$Shows,predrf)
(2432)/nrow(test)  ## accuracy 0.811 for seed 1234
#######################################################################################################


## SMOTE (SYNTHETIC MINORITY OVERSAMPLING TECHNIQUES)
library(caret)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(Shows ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received,
                               data = train,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

final_smote <- data.frame(actual = test$Shows,
                          predict(model_rf_smote, newdata = test, type = "prob"))
final_smote$predict <- ifelse(final_smote$X1 > 0.5, "1", "0")
cm_smote <- confusionMatrix(final_smote$predict, test$Shows)
cm_smote

## ROSE (RANDOM OVERSAMPLING EXAMPLES)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(Shows ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received,
                              data = train,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_rose <- data.frame(actual = test$Shows,
                         predict(model_rf_rose, newdata = test, type = "prob"))
final_rose$predict <- ifelse(final_rose$X1 > 0.5, "1", "0")
cm_rose <- confusionMatrix(final_rose$predict, test$Shows)
cm_rose

## OVERSAMPLING
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "up")

set.seed(42)
model_rf_over <- caret::train(Shows ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received,
                              data = train,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_over <- data.frame(actual = test$Shows,
                         predict(model_rf_over, newdata = test, type = "prob"))
final_over$predict <- ifelse(final_over$X1 > 0.5, "1", "0")
cm_over <- confusionMatrix(final_over$predict, test$Shows)
cm_over

## UNDERSAMPLING
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
model_rf_under <- caret::train(Shows ~ Age +  Alcoholism + Hipertension + Scholarship + SMS_received,
                               data = train,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)
final_under <- data.frame(actual = test$Shows,
                          predict(model_rf_under, newdata = test, type = "prob"))
final_under$predict <- ifelse(final_under$X1 > 0.5, "1", "0")
cm_under <- confusionMatrix(final_under$predict, test$Shows)
cm_under

## Graph comparing all the Re-Sampling Models
models <- list(#original = model_rf,
  under = model_rf_under,
  over = model_rf_over,
  smote = model_rf_smote,
  rose = model_rf_rose)

resampling <- resamples(models)
bwplot(resampling)

## Kappa is another metric for evaluating model performance. It ranges from -1 to 1 
#####################################################################################################




### insights on waitingtime variable, by separating observations with waitingtime 0 days


a = subset(newdata1,newdata1$waitingtime == 0)
datawait=newdata1[!(row.names(newdata1)%in% row.names(a)),]
dataNowait = newdata1[(row.names(newdata1)%in% row.names(a)),]

table(datawait$Shows, datawait$waitingtime)
table(dataNowait$Shows, dataNowait$waitingtime)


table(datawait$Shows)
b = 1784/(1784+4642) #percentage showing not up with at least 1 day of waiting time
table(dataNowait$Shows)
c = 163/(163+3411) #percentage showing not up with 0 day waiting time
table(newdata$Shows)
d = 1947/(1947+8053) #percentage of people showing not up regardless of waiting time

library(plotly) #install plotly first

p <- plot_ly(
  x = c("waitingime > 0", "waitingtime = 0", "average"),
  y = c(b, c, d),
  name = "insights waitingtime",
  type = "bar",
  
)%>%
  layout(yaxis = list(title = 'Percentage of NoShows'), barmode = 'stack')
  
p



