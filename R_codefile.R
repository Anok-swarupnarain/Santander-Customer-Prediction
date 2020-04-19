#set working directory
library(readr)
setwd("C:/Users/Anok/Downloads/Edwisor Santander Project")
#get working directory
getwd
x <- c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
        "MASS", "rpart", "gbm", "ROSE", 'sampling',"scales" ,'DataCombine', 'inTrees',"ROCR","Matrix")

lapply(x, require, character.only = TRUE)
rm(x)
#load Train.csv Data File as Train
train<-read.csv("train.csv",header=TRUE)
#load Test.Csv data file as Test
test<-read.csv("test.csv",header = TRUE)
#dimensions of train data
dim(train)
#dimensions of test data
dim(test)
#structure of train data
str(train)
#structure of test data
str(test)
#summary of train data
summary(train)
#summary of test data
summary(test)
#View Data frames of test and train
View(train)
View(test)
#to know type of variables in train data
sapply(train,typeof)
#to know type of variables in test data
sapply(test,typeof)
#variable names in train data
colnames(train)
#variable names in test data
colnames(test)

#convert type of target variable in train data
train$target<-as.factor(train$target)
class(train$target)
#remove ID_code from Train data
train<-train[,-1]

#Missing Value analysis
missing_val<-data.frame(apply(train,2,
                              function(x){sum(is.na(x))}))
missing_val$columns<-row.names(missing_val)
names(missing_val)[1]<-"Missing_Percentage"
missing_val$Missing_Percentage<-(missing_val$Missing_Percentage/nrow(train)) * 100
missing_val <- missing_val[order(-missing_val$Missing_Percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
sum(is.na(train))
sum(is.na(test))

#missing values are in both data sets are zero .Hence did not iterate same function to test.csv data


#Let us check for class imbalance in Train data
table<-table(train$target)/length(train$target)*100
table
#90 percent of the customers in Train data has target values 0 and 10 percent has value 1
#Visualization of Class imbalance
ggplot(train, aes_string(x = train$target)) +
  geom_bar(stat="count",fill =  "SkyBlue") + theme_bw() +
  xlab("target") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("santander transaction") +  theme(text=element_text(size=15))

#Important features
library(randomForest)
train$target<-as.factor(train$target)
rf<-randomForest(target~.,train,ntree=10,importance=TRUE)
important_variables<-importance(rf,type=2)
important_variables
#We can visually see that few of the variable like Var_81,var_26,var_53,var_12,var_139 carry important information
corr<-cor(train[,2:201])
View(corr)
#We can derive from the data that Explanatory variables are very lowly correlated

#let us split train data into train(80%) and test(20%) data sets
set.seed(1234)
train.index<-createDataPartition(train$target,p=.80,list=FALSE)
trainset<-train[train.index,]
testset<-train[-train.index,]

#logistic regression on this data
logit_model<-glm(target~.,data=trainset,family = "binomial")
summary(logit_model)
logit_Predictions <- predict(logit_model, newdata = testset[,-1], type = "response")
logit_Predictions <-ifelse(logit_Predictions > 0.5, 1,0)

ConfMatrix_LR = table(testset$target, logit_Predictions)
ConfMatrix_LR 
Accuracy<-((35476+1090)*100)/(35476+504+2929+1090)
Accuracy
#Accuracy is 91.41729
library(ROCR)
pred<-prediction(logit_Predictions,testset$target)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

accuracy.meas(testset$target,logit_Predictions)
#precision is 0.684
#recall is 0.271
#F is 0.191

#Accuracy has been 91.41729 but on the basis of ROC curve and f1 score our model is not  performing well on this imbalanced data

#let us use SMOTE to balance data
train_rose<-ROSE(target~.,data=trainset,seed = 1)$data
test_rose<-ROSE(target~.,data = testset,seed = 1)$data
table(train_rose$target)
table(test_rose$target)
#with the help SMOTE(Synthetic Minority Oversampling Technique) We are able to balance data
#In train_rose set We have 0 as target for 80036 records and 1 as target for 79965 records
#In test_rose set We have 0 as target for 20085 records and 1 as target for 19914 records


#lets apply logistic regression on SMOTE sample synthetics data
logit_model <- glm(target ~ ., data=train_rose, family = "binomial")
logit_Predictions <- predict(logit_model, newdata = test_rose[,-1], type = "response")
logit_Predictions <- ifelse(logit_Predictions > 0.5, 1, 0)

ConfMatrix_LR <- table(test_rose$target, logit_Predictions)
ConfMatrix_LR 
Accuracy<-((14349+14264)/(14349+5736+5650+14264))*100
Accuracy
#Accuracy of the model is 71.53429
library(ROCR)
pred<-prediction(logit_Predictions,test_rose$target)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

accuracy.meas(logit_Predictions,test_rose$target)

#from ROC curve and F1 curve We can conclude that logistic regression is performing 
#better on Synthetic data than Imbalanced Data


#Lets Build Random Forest Model
RF_model<-randomForest(target ~ ., trainset, importance = TRUE, ntree = 5)
RF_Predictions <- predict(RF_model, testset[,-1])
ConfMatrix_RF<-table(testset$target,RF_Predictions)
ConfMatrix_RF
Accuracy<-(35423+303)/(35423+303+3716+557)
Accuracy
#accuracy of the model is 89.3%

RF_Predictions<-as.double(RF_Predictions)
pred<-prediction(RF_Predictions,test$target)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
accuracy.meas(RF_Predictions,testset$target)
#looking at roc curve and F1 score we can assume that Our model has underporformed on Imbalanced Data

# Let's use Random Forest on synthetic data 
RF_model <- randomForest(target ~ ., train_rose, importance = TRUE, ntree = 100,seed=2)
RF_Predictions <- predict(RF_model, test_rose[,-1])

ConfMatrix_RF<-table(test_rose$target,RF_Predictions)
ConfMatrix_RF
Accuracy<-(13748+14064)/(5850+13748+14064+6337)
Accuracy
#accuracy of model with synthetic data is 69.53%
RF_Predictions<-as.double(RF_Predictions) 
pred<-prediction(RF_Predictions,test_rose$target)
perf <- ROCR::performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

accuracy.meas(RF_Predictions,test_rose$target)
# on synthetic SMOTE data Random Forest classifier model works a little better

#Lets try Naive Bayes on Imbalance data

NB_model <- naiveBayes(target ~ ., data = trainset)
NB_predictions<-predict(NB_model,testset[,2:201],type = "class")
confMatrix_NB<-table(observed=testset[,1],predicted=NB_predictions)
confMatrix_NB
Accuracy<-((35388+1493)/(35388+592+2526+1493))*100
Accuracy
#Accuracy is 92.2 % on imbalanced data
NB_predictions<-as.double(NB_predictions)
pred<-prediction(NB_predictions,testset$target)
perf<-ROCR::performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
accuracy.meas(NB_predictions,testset$target)
#performance of Naive Bayes on Imbalanced data is very poor

#Naive Bayes on SMOTE synthetic Data 
NB_model <- naiveBayes(target ~ ., data = train_rose)
NB_predictions<-predict(NB_model,test_rose[,2:201],type = "class")
confMatrix_NB<-table(observed=test_rose[,1],predicted=NB_predictions)
confMatrix_NB
Accuracy<-((15289+15236)/(15289+4796+4678+15236))*100
Accuracy
 #Accuracy on SMOTE synthetic sample is 76.3141
NB_predictions<-as.double(NB_predictions)
pred<-prediction(NB_predictions,test_rose$target)
perf<-ROCR::performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
accuracy.meas(NB_predictions,testset$target)

#accuracy on SMOTE sample sample is a lot better than that of NB on imbalanced data

#Based On Accuracy Naive Bayes is working Best .Hence choosing Naive Bayes for predicting Target class of Test.CSV
test<-read.csv("test.csv",header=T)
ID_code<-subset(test,select = ID_code)
test<-subset(test,select=-c(ID_code))
str(test)
dim(test)
View(test)
#Prediction using Naive Bayes Model
NB_predictions_test<-predict(NB_model,test[,1:200],type="class")
NB_predictions_test<-as.data.frame(NB_predictions_test)
#column bind target results with ID_code
ID_code<-cbind(ID_code,NB_predictions_test)
names(ID_code)[2] = "Target_value"
ID_code
write.csv(ID_code,"Prediction_R.csv",row.names=F)


