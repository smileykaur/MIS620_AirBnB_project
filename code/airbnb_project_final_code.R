setwd("/store/studenthome/mis620/2018Fall/wanderersinsight@gmail.com/R/AirBnb Project")

install.packages("dplyr")
install.packages("caret")
install.packages("plyr")
install.packages("sqldf")
install.packages("corrgram")
install.packages("rpart")
install.packages("ISLR")
install.packages("caret")
install.packages("maps")
install.packages("caTools")
install.packages("doParallel")
install.packages("mldr")
install.packages("e1071")
install.packages("gplots")
install.packages("pROC")
install.packages("ROCR")
install.packages("tree")
install.packages("corrgram")


#Loading All Required Libraries

library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(ISLR)
library(corrgram)
library(maps)
library(caTools)
library(doParallel)
library(pROC)
library(e1071)
library(gplots)
library(ROCR)
library(tree)
library(rpart)
library(rpart.plot)

#----------------------------------------
# Step 1: Loading Airbnb Datasets
#----------------------------------------

####### Reading Training csv file ##########
train_data<- read.csv("users_.csv")
saveRDS(train_data, "train_data.rds")

train_data<- readRDS("train_data.rds")


####### Reading Session csv file ##########
sessions_data<- read.csv("ssns.csv")
saveRDS(sessions_data, "sessions_data.rds")

sessions_data<- readRDS("sessions_data.rds")

####### Reading countries csv file ##########
countries_data<- read.csv("countries.csv")
saveRDS(countries_data , "countries_Data.rds")


####### Reading age_gender_bkts.csv  csv file ##########
age_gender_data<- read.csv("age_gender_bkts.csv")
saveRDS(age_gender_data , "age_gender_data.rds")



#-------------------------------
# Step 2: Data Exploration
#--------------------------------

head(train_data)
str(train_data)

head(sessions_data)
str(sessions_data)

head(countries_data)
str(countries_data)

head(age_gender_data)
str(age_gender_data)


### Unique testues ####

unique(train_data$date_account_created)
unique(train_data$gender)
unique(train_data$age)
unique(train_data$date_first_booking)
unique(train_data$first_affiliate_tracked)


dim(train_data)  #213451 rows    16 columns
head(train_data)
summary(train_data)
str(train_data)


# Data testidation:

# 1. To get the unique testue, missing count and missing prt, 
# 2. To get totalnumber of rows and size of data 

df.info <- function(x) {
  dat  <- as.character(substitute(x))         ##data frame name
  size <- format(object.size(x), units="Mb")  ##size of data frame in Mb
  
  ##column information
  column.info <- data.frame( column        = names(sapply(x, class)),
                             class         = sapply(x, class),
                             unique.testues = sapply(x, function(y) length(unique(y))),
                             missing.count = colSums(is.na(x)),
                             missing.pct   = round(colSums(is.na(x)) / nrow(x) * 100, 2))
  
  row.names(column.info) <- 1:nrow(column.info)
  
  list(data.frame     = data.frame(name=dat, size=size),
       dimensions     = data.frame(rows=nrow(x), columns=ncol(x)),
       column.details = column.info)
}

# Descriptive Statistics on training data
df.info(train_data)

#----------------------------------------
# Step 3: Data Visualization
#----------------------------------------

#CORRELATION

corrgram(train_data, order=TRUE,main="Airbnb correlation matrix",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

head(airbnb_cleaned_train_df)

install.packages("corrplot")
library(corrplot)
corrplot(train_data)


corrgram(airbnb_cleaned_train_df)

corrplot(airbnb_cleaned_train_df)


#-------------------------------
# Step 4 : Data Cleaning
#--------------------------------


# 1. Cleaning date_account_created feature
#--------------------------------------------------
#selecting data which is of year greater than 2014 so we will be able to merge with sessions file
cleaned_train_data <- train_data[as.integer(substr(train_data$date_account_created,1,4)) >= 2014,]
saveRDS(cleaned_train_data, "cleaned_train_data.rds")

#cleaned_train_data <- readRDS("cleaned_train_data.rds")
summary(cleaned_train_data$date_account_created)



# 2. Cleaning age feature
#--------------------------------------------------
unique(cleaned_train_data$age)
unique(cleaned_test_data$age)

# Making age > 1996 and < 1906 as NA
# Making age >110 as NA
cleaned_train_data$age[cleaned_train_data$age >= 1996] <- NA
cleaned_train_data$age[cleaned_train_data$age <= 1906 & cleaned_train_data$age >= 110] <- NA
summary(cleaned_train_data$age)



#calculating age for data having current year as testue in age 
convertyear <- function(age){
  if (is.na(age)) {
    return (age)
  }
  else if(age > 1906 & age < 1996) {
    trueage <- 2014 - age
    return (trueage)
  }
  else {
    return (age)
  }
}

trueagetemp <- sapply(cleaned_train_data$age, convertyear)
cleaned_train_data$age <- trueagetemp

summary(cleaned_train_data$age)


#Let's assume that age testues less than 18 are faulty input 
cleaned_train_data$age[cleaned_train_data$age < 18] <- NA
summary(cleaned_train_data$age)


# 3. Cleaning gender feature
#--------------------------------------------------
#gender: convert unknown to NA so that R can automatically process it. 
cleaned_train_data$gender[cleaned_train_data$gender == "-unknown-"] <- NA
summary(cleaned_train_data$gender)

dim(cleaned_train_data)  #76466 rows    16 columns



# 4. Cleaning date_first_booking feature
#--------------------------------------------------

cleaned_train_data$date_account_created <- as.character(cleaned_train_data$date_account_created) 
cleaned_train_data$date_first_booking <- as.character(cleaned_train_data$date_first_booking)
cleaned_train_data$date_first_booking <- ifelse(cleaned_train_data$date_first_booking == "", cleaned_train_data$date_account_created, cleaned_train_data$date_first_booking)

cleaned_train_data$date_account_created <- as.Date(cleaned_train_data$date_account_created)
cleaned_train_data$date_first_booking <- as.Date(cleaned_train_data$date_first_booking)

summary(cleaned_train_data)


#cleaned_train_data <- cleaned_train_data[,-c(1)]


# 5. Cleaning secs_elapsed feature in sessions csv file data
#------------------------------------------------------------

sessions_data <- readRDS("sessions_data.rds")
summary(sessions_data)


#replace NA in secs_elapsed with average time
sessions_data$secs_elapsed[is.na(sessions_data$secs_elapsed)] <- mean(na.omit(sessions_data$secs_elapsed))


#group by function gave error due to plyr package hence detaching it 
detach(package:plyr)
library(dplyr)

# 6. Feature Engineering- Creating different feature from sessions csv file
#------------------------------------------------------------------------------
session_visitors<- sessions_data  %>% group_by(user_id) %>% summarize(totalActions = length(action), 
                                                                      uniqueActions = length(unique(action)),
                                                                      uniqueAction_type = length(unique(action_type)),
                                                                      uniqueAction_detail = length(unique(action_detail)),
                                                                      freqAction_detail = names(which.max(table(action_detail))),
                                                                      device = names(which.max(table(device_type))),
                                                                      time_sec = sum(secs_elapsed))
saveRDS(session_visitors, "session_visitors.rds")

session_visitors <- readRDS("session_visitors.rds")


summary(session_visitors)
dim(session_visitors)

sessions_data$user_id <- as.character(sessions_data$user_id)
str(sessions_data)


# 7. Merging train csv file data with sessions csv file data
#------------------------------------------------------------
merged_df <- merge(cleaned_train_data, session_visitors, by.x="id", by.y="user_id") 


str(merged_df)
summary(merged_df)

merged_df<-na.omit(merged_df)

summary(merged_df)
dim(merged_df) #34342 rows    23 cols


# 8. Near Zero Variance
#-------------------------------------------------------------------

#identify variables with near or zero variance
nzv <- nearZeroVar(merged_df, saveMetrics= TRUE) #saving metrics to view variable details
nzv
#nzv[nzv$nzv,]

dim(merged_df) #342 variables to start

nzv <- nearZeroVar(merged_df) #no metrics just return identified columns indexes
nzv

#remove or filter columns with nearZeroVar
filteredDescr <- merged_df[, -nzv]
dim(filteredDescr) #297 columns remaining

merged_df <- filteredDescr


dim(merged_df)  #34342    22
df.info(merged_df)
summary(merged_df)


# 9. Dealing with Unknown and Blank testues
#-------------------------------------------------------------------

# a. first_affiliate_tracked  # convert blank to unknown

table(merged_df$first_affiliate_tracked) 
levels(merged_df$first_affiliate_tracked)[levels(merged_df$first_affiliate_tracked) == ""] <- "Unknown"


# b. freqAction_detail
table(merged_df$freqAction_detail)  #  blank to -unknown-
merged_df$freqAction_detail[merged_df$freqAction_detail == ""] <- "Unknown"


str(merged_df)
table(merged_df$first_browser)

# c. country_destination
levels(merged_df$country_destination)[levels(merged_df$country_destination) != "NDF"] <- "DF"

table(merged_df$country_destination)
table(merged_df$signup_method)
table(merged_df$first_browser)



# 10. Writing out merged & cleaned airbnb training data into CSV file
#-------------------------------------------------------------------

#writing out cleaned dataframe into csv file
write.csv(merged_df, file= "Airbnb_cleaned.csv")


airbnb_cleaned_train_df<- read.csv("Airbnb_cleaned.csv")
saveRDS(airbnb_cleaned_train_df, "airbnb_cleaned_train_df.rds")

head(airbnb_cleaned_train_df)

dim(airbnb_cleaned_train_df)
table(airbnb_cleaned_train_df$first_browser)  #27 levels


df.info(airbnb_cleaned_train_df)

# 11. Removing unwanted predictors from cleaned airbnb training data
#-------------------------------------------------------------------
airbnb_model_train_df <- airbnb_cleaned_train_df[,-c(1,2,3,5)]

str(airbnb_model_train_df)

dim(airbnb_model_train_df)  #34342 rows    19 cols



#---------------------------
# Step 5: Model Building
#---------------------------


# 1. Creating dummy variables for features with datatype as Factor
#----------------------------------------------------------------------


#gender,signup_method,language,affiliate_channel, affiliate_provider, first_Affiliate_tracked,signup_app,
#first_device_type,first_browser,country_destination, freqAction_Detail, device

head(airbnb_model_train_df)

# Encode categorical testues
#lets relevel the Default factor so Yes becomes the Positive class by default (Sensitivity)

#airbnb_model_train_df$country_destination <- relevel(airbnb_model_train_df$country_destination, ref="DF")

airbnb.dmodel <- dummyVars( ~ ., data=airbnb_model_train_df, fullRank=T)
airbnb.d <- as.data.frame(predict(airbnb.dmodel, airbnb_model_train_df))
saveRDS(airbnb.d ,"airbnb_dummyvars.rds")

dim(airbnb.d)


airbnb.d$country_destination <- airbnb_model_train_df$country_destination #copy back DV

#creating test and training set as example

#70% train and 30% test

set.seed(44)
trainIndex <- createDataPartition(airbnb.d$country_destination, p=.7, list=F)
airbnb.train <- airbnb.d[trainIndex,]
airbnb.test <- airbnb.d[-trainIndex,]

str(airbnb_model_train_df)

str(airbnb.d)
str(airbnb.dmodel)
str(airbnb_model_train_df)

str(airbnb.train)


set.seed(44)
trainIndex <- createDataPartition(airbnb_model_train_df$country_destination, p=.7, list=F)
airbnb.org.train <- airbnb_model_train_df[trainIndex,]
airbnb.org.test <- airbnb_model_train_df[-trainIndex,]


dim(airbnb.org.train)
dim(airbnb.org.test)



# 2. Cross Validation
#------------------------------------------------------------

#setup control function for resampling and two class classification performance
#using 10 fold cross testidation
ctrl <- trainControl(method = "cv", number=10, summaryFunction=twoClassSummary,
                     classProbs=T, savePredictions=T) #saving predictions from each resample fold



# 3. Parallel Processing
#------------------------------------------------------------
#Register core backend, using 5 cores
cl <- makeCluster(5)
registerDoParallel(cl)

#list number of workers
getDoParWorkers()


table(airbnb_model_train_df$country_destination)


# A. LINEAR MODEL
#--------------------------------
# 1.logistic regression
#--------------------------------
set.seed(44)
airbnb.glm <-  train(country_destination ~ ., data=airbnb.train, method="glm", family="binomial", metric="ROC", trControl=ctrl)
saveRDS(airbnb.glm, "airbnb.glm.rds")

summary(airbnb.glm)
varImp(airbnb.glm)
getTrainPerf(airbnb.glm)

#calculate resampled accuracy/confusion matrix using extracted predictions from resampling
confusionMatrix(airbnb.glm$pred$pred, airbnb.glm$pred$obs) #take averages

#For test data
test.pred.prob <- predict.lm(airbnb.glm, airbnb.test)
pred <- predict(airbnb.glm, newdata=airbnb.test, type="prob")
confusionMatrix(data=pred, airbnb.test$c)

#ROC
airbnb.glm.roc<- roc(response= airbnb.glm$pred$obs, predictor=airbnb.glm$pred$DF)
plot(airbnb.glm.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 2.linear discriminant analysis
#--------------------------------
set.seed(44)
airbnb.lda <-  train(country_destination ~ ., data=airbnb.train, method="lda", metric="ROC", trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.lda ,"airbnb.lda.rds")

summary(airbnb.lda)
varImp(airbnb.lda)
confusionMatrix(airbnb.lda$pred$pred, airbnb.lda$pred$obs) #take averages
getTrainPerf(airbnb.lda)

#ROC
airbnb.lda.roc<- roc(response= airbnb.lda$pred$obs, predictor=airbnb.lda$pred$DF)
plot(airbnb.lda.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 3. quadratic distriminant analysis
#--------------------------------
set.seed(44)
airbnb.qda <-  train(country_destination ~ ., data=airbnb.train, method="qda", metric="ROC", 
                     trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.qda , "airbnb.qda.rds")
summary(airbnb.qda)

confusionMatrix(airbnb.qda$pred$pred, airbnb.qda$pred$obs) #take averages
getTrainPerf(airbnb.qda)


#ROC
airbnb.qda.roc<- roc(response= airbnb.qda$pred$obs, predictor=airbnb.qda$pred$DF)
plot(airbnb.qda.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 4. k nearest neighbors classification
#--------------------------------s
set.seed(44) 
airbnb.knn <-  train(country_destination ~ ., data=airbnb.org.train, method="knn", metric="ROC", trControl=ctrl, tuneLength=10) #let caret decide 10 best parameters to search
saveRDS(airbnb.knn, "airbnb.knn.rds")

airbnb.knn <- readRDS("airbnb.knn.rds")

summary(airbnb.knn)
plot(airbnb.knn)
getTrainPerf(airbnb.knn)

confusionMatrix(airbnb.knn$pred$pred, airbnb.knn$pred$obs) #make sure to select resamples only for optimal parameter of K


# predict with testdata
airbnb_test_knn <- predict (airbnb.knn,airbnb.org.test,type="prob")
airbnb_test_knn.class <- predict(airbnb.knn, airbnb.org.test)
saveRDS(results.class ,"knn.test_results.rds")

confusionMatrix(airbnb_test_knn.class, airbnb.org.test$country_destination)
varImp(airbnb.knn)


#ROC
airbnb.knn.roc<- roc(response= airbnb.knn$pred$obs, predictor=airbnb.knn$pred$DF)
plot(airbnb.knn.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 5. Naive Bayes
#--------------------------------
airbnb.nb <-  train(country_destination ~ ., data=airbnb.org.train, method="nb", family="binomial",
                    metric="ROC", trControl=ctrl, tuneLength=10)
saveRDS(airbnb.nb, "airbnb.nb.rds")


airbnb.nb <- readRDS("airbnb.nb.rds")

getTrainPerf(airbnb.nb)
confusionMatrix(airbnb.nb$pred$pred, airbnb.nb$pred$obs) 

# predict with testdata
airbnb_test_nb <- predict (airbnb.nb,airbnb.org.test,type="prob")
airbnb_test_nb.class <- predict(airbnb.nb, airbnb.org.test)
saveRDS(results.class ,"nb.test_results.rds")

confusionMatrix(airbnb_test_nb.class, airbnb.org.test$country_destination)
varImp(airbnb.nb)


#ROC
airbnb.nb.roc<- roc(response= airbnb.nb$pred$obs, predictor=airbnb.nb$pred$DF)
plot(airbnb.nb.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# B. NON-LINEAR MODEL
#--------------------------------

# 6. DECISION TREE
#-------------------
set.seed(44)
airbnb.dt <- train(country_destination ~ ., data=airbnb.org.train, method="rpart",
                   tuneLength=4, trControl=ctrl)
head(airbnb.dt)
saveRDS(airbnb.dt, "airbnb.dt.rds")

airbnb.dt <- readRDS("airbnb.dt.rds")


summary(airbnb.dt)
getTrainPerf(airbnb.dt)
plot(airbnb.dt,main='Decision Tree')

confusionMatrix(airbnb.dt$pred$pred, airbnb.dt$pred$obs) 


#test data
airbnb.dt.test <- predict(airbnb.dt, newdata = airbnb.org.test,type="prob")
head(airbnb.dt.test)
airbnb.dt.test.class <- predict(airbnb.dt , airbnb.org.test)

saveRDS(airbnb.dt.test.class ,"airbnb.dt.test.class.rds")


confusionMatrix(airbnb.dt.test.class , airbnb.org.test$country_destination)


#let draw ROC curve of training and test performance of decision tree  model
test.log.roc<- roc(response= airbnb.org.test$country_destination, predictor=airbnb.dt.test[[1]]) #assumes postive class Yes is reference level
plot(test.log.roc, legacy.axes=T)


#ROC
airbnb.dt.roc<- roc(response= airbnb.dt$pred$obs, predictor=airbnb.dt$pred$DF)
plot(airbnb.dt.roc, legacy.axes=T,col="Blue" ,asp = NA) 




#create tree
airbnb.dt.rpart <- rpart(country_destination ~ ., data=airbnb_model_train_df)
saveRDS(airbnb.dt.rpart, "airbnb.dt.rpart.rds")



rpart.plot(airbnb.dt.rpart)
varImp(airbnb.dt)



# 7. BAGGING TREE
#-------------------
set.seed(44)
airbnb.bt <- train(country_destination ~ ., data=airbnb.org.train, method="treebag",tuneLength=4, trControl=ctrl)
saveRDS(airbnb.bt, "airbnb.bt.rds")

airbnb.bt <- readRDS("airbnb.bt.rds")

airbnb.bt <- readRDS("airbnb.bt")

#summary(airbnb.bt)
getTrainPerf(airbnb.bt)
confusionMatrix(airbnb.bt$pred$pred, airbnb.bt$pred$obs) 


#Test Data
airbnb.bt.test <- predict(airbnb.bt, newdata = airbnb.org.test,type="prob")
airbnb.bt.test.class <- predict(airbnb.bt , airbnb.org.test)
saveRDS(airbnb.bt.test.class, "airbnb.bt.test.class.rds")

confusionMatrix(airbnb.bt.test.class , airbnb.test$country_destination)


varImp(airbnb.bt)


#ROC
airbnb.bt.roc<- roc(response= airbnb.bt$pred$obs, predictor=airbnb.bt$pred$DF)
plot(airbnb.bt.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 8. RANDOM FOREST
#-------------------

set.seed(44)
airbnb.rf <- train(country_destination ~ ., data=airbnb.org.train, method="rf",tuneLength=4,trControl=ctrl, allowParallel = TRUE)

saveRDS(airbnb.rf, "airbnb.rf.rds")

airbnb.rf <- readRDS("airbnb.rf.rds")

plot(airbnb.rf,main='Random Forest')
confusionMatrix(airbnb.rf$pred$pred, airbnb.rf$pred$obs) 

p.airbnb.rf <- predict(airbnb.rf, newdata = airbnb.org.test, type="prob")
R.airbnb.test.rf <- postResample(p.airbnb.rf, airbnb.org.test$country_destination)
saveRDS(R.airbnb.test.rf, "R.airbnb.test.rf.rds")


confusionMatrix(p.airbnb.rf , airbnb.org.test$country_destination)

varImp(airbnb.rf)


#ROC
airbnb.rf.roc<- roc(response= airbnb.rf$pred$obs, predictor=airbnb.rf$pred$DF)
plot(airbnb.rf.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 9. BOOSTING
#-------------------
set.seed(44)
airbnb.boost <- train(country_destination ~ ., data=airbnb.org.train, method="gbm",tuneLength=4,trControl=ctrl)
saveRDS(airbnb.boost , "airbnb.boost.rds")

airbnb.boost <- readRDS("airbnb.boost.rds")


summary(airbnb.boost)
getTrainPerf(airbnb.boost)
plot(airbnb.boost,main="Boosting")


confusionMatrix(airbnb.boost$pred$pred, airbnb.boost$pred$obs) 

varImp(airbnb.boost)

#for test data
p.airbnb.boost <- predict(airbnb.boost, newdata = airbnb.org.test, type="prob")
R.airbnb.test.boost <- postResample(p.airbnb.boost, airbnb.org.test$country_destination)
saveRDS(R.airbnb.test.boost,"R.airbnb.test.boost.rds")
R.airbnb.test.boost


#ROC
airbnb.boost.roc<- roc(response= airbnb.boost$pred$obs, predictor=airbnb.boost$pred$DF)
plot(airbnb.boost.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 10. Neural Network
#-------------------
modelLookup("nnet")
m.nnet <- train(country_destination~ ., 
                trControl = ctrl,
                metric = "ROC", #using AUC to find best performing parameters
                preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                data = airbnb.org.train, 
                method = "nnet")

saveRDS(m.nnet , "m.nnet.rds")

m.nnet <-readRDS("m.nnet.rds")

confusionMatrix(m.nnet$pred$pred, m.nnet$pred$obs) 


#for test data
plot(m.nnet)
p.test_nnet <- predict(m.nnet,airbnb.org.test, type="prob")
saveRDS(p.nnet, "p.nnet.rds")
confusionMatrix(p.test_nnet,airbnb.org.test$country_destination)

varImp(m.nnet)


#ROC
m.nnet.roc<- roc(response= m.nnet$pred$obs, predictor=m.nnet$pred$DF)
plot(m.nnet.roc, legacy.axes=T,col="Blue" ,asp = NA) 


# 11. SVM
#-------------------
##svm with radial kernel
set.seed(44)
modelLookup("svmRadial")
m.svm <- train(country_destination~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = airbnb.org.train, 
               method = "svmRadial")
saveRDS(m.svm , "m.svm.rds")

m.svm <-readRDS("m.svm.rds")

plot(m.svm)
confusionMatrix(m.svm$pred$pred, m.svm$pred$obs) 


#for test data
p.test_svm<- predict(m.svm,airbnb.org.test, type="prob")
saveRDS(p.svm,"p.svm.RDS")
confusionMatrix(p.test_svm,airbnb.org.test$country_destination)

varImp(m.svm)


#ROC
m.svm.roc<- roc(response= m.svm$pred$obs, predictor=m.svm$pred$DF)
plot(m.svm.roc, legacy.axes=T,col="Blue" ,asp = NA) 


#Closing cluster (releasing computers cores)
stopCluster(cl)


#---------------------------
# Step 6: Model Etestuation
#---------------------------

# 1. Linear Models
#---------------------
models.linear<- list("LR"=airbnb.glm)

# 2. Non-Linear Models
#---------------------
models.nonlinear<- list("NaiveBayes"= airbnb.nb, "Knn"=airbnb.knn, "DecisionTree"=airbnb.dt, "Bagging" = airbnb.bt, "RandomForest"= airbnb.rf, "Boosting"=airbnb.boost, "SVM"=m.svm ,"Neuralnetwork"=m.nnet )


# Result matrix: LINEAR
summary(models.linear)

# Plot model performances
bwplot(airbnb.resamples.linear, metric="ROC")
bwplot(airbnb.resamples.linear, metric="Sens")
bwplot(airbnb.resamples.linear, metric="Spec")


# Result matrix: NON-LINEAR
airbnb.resamples.nonlinear<- resamples(models.nonlinear)
summary(airbnb.resamples.nonlinear)

# Plot model performances
bwplot(airbnb.resamples.nonlinear, metric="ROC")
bwplot(airbnb.resamples.nonlinear, metric="Sens")
bwplot(airbnb.resamples.nonlinear, metric="Spec")
bwplot(airbnb.resamples.nonlinear, metric="Acc")


# MODEL Performances- ALL MODELS COMBINED
models.all<- list( "NaiveBayes"= airbnb.nb, "Knn"=airbnb.knn, "DecisionTree"=airbnb.dt, "Bagging" = airbnb.bt, "RandomForest"= airbnb.rf, "Boosting"=airbnb.boost, "SVM"=m.svm ,"Neuralnetwork"=m.nnet )

airbnb.resamples.all<- resamples(models.all)
summary(airbnb.resamples.all)

bwplot(airbnb.resamples.all, metric="ROC")
bwplot(airbnb.resamples.all, metric="Sens")
bwplot(airbnb.resamples.all, metric="Spec")


# ROC
#---------------------

#Training Data
#calculate ROC curves on resampled data

airbnb.nb.roc<- roc(response= airbnb.nb$pred$obs, predictor=airbnb.nb$pred$DF)
airbnb.knn.roc<- roc(response= airbnb.dt$pred$obs, predictor=airbnb.dt$pred$DF)
airbnb.dt.roc<- roc(response= airbnb.dt$pred$obs, predictor=airbnb.dt$pred$DF)
airbnb.bt.roc<- roc(response= airbnb.bt$pred$obs, predictor=airbnb.bt$pred$DF)
airbnb.boost.roc<- roc(response= airbnb.boost$pred$obs, predictor=airbnb.boost$pred$DF)
airbnb.rf.roc<- roc(response= airbnb.rf$pred$obs, predictor=airbnb.rf$pred$DF)
airbnb.m.nnet.roc<- roc(response= m.nnet$pred$obs, predictor=m.nnet$pred$DF)
airbnb.m.svm.roc<- roc(response= m.svm$pred$obs, predictor=m.svm$pred$DF)

#build to combined ROC plot with resampled ROC curves


plot.new()

par(pty="s")
plot(airbnb.knn.roc, legacy.axes=T,col="Orange" ,asp = NA) 
plot(airbnb.nb.roc, add=T,col="Purple" ,asp = NA) 
plot(airbnb.dt.roc,add=T, col="Maroon" ,asp = NA)      #ylim=c(0:1),xlim=c(0:1)
plot(airbnb.bt.roc, add=T, col="Blue" ,asp = NA)
plot(airbnb.boost.roc, add=T, col="Green" ,asp = NA)
plot(airbnb.rf.roc, add=T, col="Red" ,asp = NA)
plot(airbnb.m.nnet.roc, add=T, col="Pink" ,asp = NA)
plot(airbnb.m.svm.roc, add=T, col="Yellow" ,asp = NA)



#par(xpd = T, mar = par()$mar + c(0,0,0,7))

#text(0.4, 0.43, labels=sprintf("AUC: %0.3f", auc(airbnb.nb.roc)), col="red")
legend("bottomright", legend=c("Knn", "NaiveBayes", "DecisionTree", "Bagging" , "Boosting" , "RandomForest" ,"NeuralNet" , "SVM"),
       col=c("Orange","Purple","Maroon" , "blue","green","red" ,"Pink" ,"Yellow"),lty=1 , cex=0.55)



# Traning data AUC
airbnb.nb.auc<- auc(response= airbnb.nb$pred$obs, predictor=airbnb.nb$pred$DF)
airbnb.knn.auc<- auc(response= airbnb.dt$pred$obs, predictor=airbnb.dt$pred$DF)
airbnb.dt.auc<- auc(response= airbnb.dt$pred$obs, predictor=airbnb.dt$pred$DF)
airbnb.bt.auc<- auc(response= airbnb.bt$pred$obs, predictor=airbnb.bt$pred$DF)
airbnb.boost.auc<- auc(response= airbnb.boost$pred$obs, predictor=airbnb.boost$pred$DF)
airbnb.rf.auc<- auc(response= airbnb.rf$pred$obs, predictor=airbnb.rf$pred$DF)
airbnb.m.nnet.auc<- auc(response= m.nnet$pred$obs, predictor=m.nnet$pred$DF)
airbnb.m.svm.auc<- auc(response= m.svm$pred$obs, predictor=m.svm$pred$DF)

#Training data AUC value
airbnb.test.svm.auc
airbnb.knn.auc
airbnb.dt.auc
airbnb.bt.auc
airbnb.boost.auc
airbnb.rf.auc
airbnb.m.nnet.auc
airbnb.m.svm.auc


#Test Data
#calculate ROC curves on resampled data
head(airbnb.test)

airbnb.test.nb.roc<- roc(response= airbnb.test$country_destination, predictor=airbnb_test_nb[[1]])
airbnb.test.knn.roc<- roc(response= airbnb.test$country_destination, predictor=airbnb_test_knn[[1]])
airbnb.test.dt.roc<- roc(response= airbnb.test$country_destination, predictor=airbnb.dt.test[[1]])
airbnb.test.bt.roc<- roc(response= airbnb.test$country_destination, predictor=airbnb.bt.test[[1]])
airbnb.test.boost.roc<- roc(response= airbnb.test$country_destination, predictor=p.airbnb.boost[[1]])
airbnb.test.rf.roc<- roc(response= airbnb.test$country_destination, predictor=p.airbnb.rf[[1]])
airbnb.test.nnet.roc<- roc(response= airbnb.test$country_destination, predictor=p.test_nnet[[1]])
airbnb.test.svm.roc<- roc(response= airbnb.test$country_destination, predictor=p.test_svm[[1]])

#Test data - AUC value
airbnb.test.nb.auc<- auc(response= airbnb.test$country_destination, predictor=airbnb_test_nb[[1]])
airbnb.test.knn.auc<- auc(response= airbnb.test$country_destination, predictor=airbnb_test_knn[[1]])
airbnb.test.dt.auc<- auc(response= airbnb.test$country_destination, predictor=airbnb.dt.test[[1]])
airbnb.test.bt.auc<- auc(response= airbnb.test$country_destination, predictor=airbnb.bt.test[[1]])
airbnb.test.boost.auc<- auc(response= airbnb.test$country_destination, predictor=p.airbnb.boost[[1]])
airbnb.test.rf.auc<- auc(response= airbnb.test$country_destination, predictor=p.airbnb.rf[[1]])
airbnb.test.nnet.auc<- auc(response= airbnb.test$country_destination, predictor=p.test_nnet[[1]])
airbnb.test.svm.auc<- auc(response= airbnb.test$country_destination, predictor=p.test_svm[[1]])


#Print the AUC values here
airbnb.test.nb.auc
airbnb.test.knn.auc
airbnb.test.dt.auc
airbnb.test.bt.auc
airbnb.test.boost.auc
airbnb.test.rf.auc
airbnb.test.nnet.auc
airbnb.test.svm.auc

#build to combined ROC plot with resampled ROC curves


plot.new()

par(pty="s")
plot(airbnb.test.knn.roc, legacy.axes=T,col="Orange" ,asp = NA) 
plot(airbnb.test.nb.roc, add=T,col="Purple" ,asp = NA) 
plot(airbnb.test.dt.roc,add=T, col="Maroon" ,asp = NA)      #ylim=c(0:1),xlim=c(0:1)
plot(airbnb.test.bt.roc, add=T, col="Blue" ,asp = NA)
plot(airbnb.test.boost.roc, add=T, col="Green" ,asp = NA)
plot(airbnb.test.rf.roc, add=T, col="Red" ,asp = NA)
plot(airbnb.test.nnet.roc, add=T, col="Pink" ,asp = NA)
plot(airbnb.test.svm.roc, add=T, col="Yellow" ,asp = NA)


legend("bottomright", legend=c("Knn", "NaiveBayes", "DecisionTree", "Bagging" , "Boosting" , "RandomForest" ,"NeuralNet" , "SVM"),
       col=c("Orange","Purple","Maroon" , "blue","green","red" ,"Pink" ,"Yellow"),lty=1 , cex=0.45)



#---------------Additional code -----------

# 7. Unsupervised Learning: PCA
#--------------------------------------

library(caret)

#lets remove the reponse variable
airbnb.d$country_destination <- NULL

#lets fit PCA making sure to scale
airbnb.pca <- prcomp(airbnb.d, scale=TRUE) 

#giving the numerical data hitters.d

#view scaling centers and SD
airbnb.pca$center
airbnb.pca$scale

#loading the data
#view the pca loadings one PC for each 19 variables
airbnb.pca$rotation
#think of it as rubex cube
#we dont care about sign of the testue

#view biplot of first two components
biplot(airbnb.pca, scale=0)


#variance explained by each component, squaring standard deviation 
pca.var<- airbnb.pca$sdev^2

#proportion of variance explained
pve<- pca.var/ sum(pca.var)

#scree plot, variance explained by component
plot(pve, xlab="PCA", ylab="Prop of Variance Explained", ylim=c(0,1), type='b')

#cumulative variance explained
#scree plot, variance explained by component
plot(cumsum(pve), xlab="PCA", ylab="Cumulative Prop of Variance Explained", ylim=c(0,1), type='b')

#grabe the first five PCs
airbnb.pca$x[,1:5]


h.pca <- data.frame(airbnb.pca$x[,1:5])
cbind(airbnb.d$Salary,h.pca)

#in R mostly data is sorted.

#shorter version of summarizing variance
summary(airbnb.pca)
plot(airbnb.pca)


#clustering k-means
set.seed(22)
#k = 3 with 20 random initilizations 
#20 different starting points will be taken
#iter.max defaults to 10 for complex data you can increase it to more ex:50
airbnb.kmeans.2 <- kmeans(airbnb.d, 2, nstart=20)
airbnb.kmeans.2
#seeking to reduce within sum of squares error
airbnb.kmeans.2$tot.withinss

#lets try k 3
airbnb.kmeans.3 <- kmeans(airbnb.d, 3, nstart=20)
airbnb.kmeans.3$tot.withinss
#total within sum of squares going down is good, 3 better than 2 here

#finding optimal K by wss, try k 1 through 10
wss <- numeric(10)  #empty vectors with 10 positions
for (k in 1:10) {
  clust <- kmeans(airbnb.d, centers=k, nstart=25)
  
  wss[k] <- sum(clust$withinss)
}
#vew plot of wss by cluster size
#3 or 4 seem ideal

plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 
#3 or 4 point is good as a cluster

install.packages("cluster")
library("cluster")

clusplot(airbnb.d, airbnb.kmeans.3$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


#plot the clusters by first two PCAs to reduced dimensons

#step 1
#hierarchical clusters, scaling before calculating distance and clustering)
airbnb.hc.complete <- hclust(dist(scale(airbnb.d)), method="complete")

#step 2
#plot the dendograms
plot(airbnb.hc.complete)
#select vertical cut to create clusters
#step 3
cutree(airbnb.hc.complete, 4) #4 ggroups/clusters
#step 4
cutree(airbnb.hc.complete, h=1500) #or by height  and can change height to 15
