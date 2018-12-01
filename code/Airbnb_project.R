
setwd("/store/studenthome/mis620/2018Fall/wanderersinsight@gmail.com/R/AirBnb Project")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("htmlTable")
install.packages("knitr")
install.packages("viridis", dependencies = TRUE)
install.packages("htmlwidgets")
install.packages("caret")
install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("stringi")

install.packages("chron")
install.packages("sqldf")
install.packages("corrgram")
install.packages("recipes")
install.packages("ipred")
install.packages("rpart")
install.packages("ISLR")
install.packages("caret")
install.packages("mice")
install.packages("dplyr")
install.packages("maps")
install.packages("caTools")
install.packages("doParallel")
install.packages("mldr")



#Loading All Required Libraries

setwd(("/Users/apurvprakash/Desktop/SDSU_MSIS2017/MIS 620/Group Project/Airbnb_Dataset"))


#Loading All Required Libraries
library(ggplot2)
library(plyr)
library(gridBase)
library(grid)
library(gridExtra)
library(lattice)



library(ggplot2)
library(plyr)
library(gridBase)
library(grid)
library(gridExtra)
library(lattice)
library(dplyr)
library(reshape2)
library(stringi)


library(caret)
library(shiny)
library(miniU)
library(digest)
library(htmltools)
library(questionr)
library(pkgload)
library(pkgbuild)
library(callr)
library(base64enc)
library(recipes)

library(ISLR)
library(caret)
library(mice)
library(chron)
library(sqldf)
library(corrgram)
library(ggplot2)
library(scales)
library(dplyr)
library(maps)
library(caTools)
library(doParallel)


library(ggplot2)
library(plyr)
library(gridBase)
library(grid)
library(gridExtra)
library(lattice)



#----------------------------------------
# Step 1: Loading Airbnb Datasets
#----------------------------------------

####### Reading Training csv file ##########
train_data<- read.csv("train_users_2.csv")
saveRDS(train_data, "train_data.rds")


#train_data <- train_data[1:50000,]
#head(train_subset)


####### Reading Session csv file ##########
sessions_data<- read.csv("ssns.csv")
saveRDS(sessions_data, "sessions_data.rds")



####### Reading countries csv file ##########
countries_data<- read.csv("countries.csv")
saveRDS(countries_data , "countries_Data.rds")


####### Reading age_gender_bkts.csv  csv file ##########
age_gender_data<- read.csv("age_gender_bkts.csv")
saveRDS(age_gender_data , "age_gender_data.rds")



####### Reading sample submission  csv file ##########
sample <- read.csv("sample_submission_NDF.csv")
saveRDS(sample ,"sample.rds")




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

head(sample)
str(sample)



### Unique Values ####

unique(train_data$date_account_created)
unique(train_data$gender)
unique(train_data$age)
unique(train_data$date_first_booking)
unique(train_data$affiliate_channel)


dim(train_data)  #213451 rows    16 columns
head(train_data)
summary(train_data)
str(train_data)


# Data Validation:

# 1. To get the unique value, missing count and missing prt, 
# 2. To get totalnumber of rows and size of data 

df.info <- function(x) {
  dat  <- as.character(substitute(x))         ##data frame name
  size <- format(object.size(x), units="Mb")  ##size of data frame in Mb
  
  ##column information
  column.info <- data.frame( column        = names(sapply(x, class)),
                             class         = sapply(x, class),
                             unique.values = sapply(x, function(y) length(unique(y))),
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

#BAR CHART

#Box Plot


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


# 1. Age
# To see if there exists any outliers 
boxplot(train_data$age)
#To check if outliers exist in all the gender cases.
qplot(x = gender, y = age, data = train_data, 
      geom = "boxplot") 
#Age by destination country
ggplot(train_data, aes(country_destination, age)) +
  geom_boxplot(aes()) + ylim(25,50)

ggplot(train_data, aes(age)) +
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  geom_histogram(data=subset(train_data,age==20), color = "black", fill="red", binwidth = 1) +
  scale_x_continuous(limits = c(15, 25), breaks = seq(15, 25, 1)) +
  facet_wrap(~country_destination, ncol = 3, scales = "free")

# 2. Gender
ggplot(train_data, aes(x = gender)) + geom_bar()

# 3. Date first Booking
# Converting to Date Format
train_data$date_first_booking <- as.character(train_data$date_first_booking)
# copying the values of date_account_created to the column with date_first_booking
train_data$date_account_created <- as.character(train_data$date_account_created)
train_data$date_first_booking <- ifelse(train_data$date_first_booking == "", train_data$date_account_created, train_data$date_first_booking)
train_data$date_account_created <- as.Date(train_data$date_account_created)
train_data$date_first_booking <- as.Date(train_data$date_first_booking)    

summary(train_data$date_account_created)
#Considering Month (We create a new column)
train_data$month_first_book <- factor(format(train_data$month_first_book, format = "%B"), 
                                      levels = c("January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September",
                                                 "October", "November", "December"))
summary(train_data$month_first_book)
qplot(x =month_first_book, data = train_data)



# 8. Signup_Method
unique(train_data$signup_method)
summary(train_data$signup_method)
count(train_data$signup_method, vars = NULL)

#frequency plot of signup_method
qplot(x = signup_method, data = train_data)

# frequenct plot for each destination country of signup_method- Without US
ggplot(subset(train_data, country_destination != "NDF" & country_destination != "US"  & signup_method == "facebook"), aes(x = signup_method)) +
  geom_bar()+
  facet_wrap(~country_destination)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# #frequenct plot for each destination country of signup_method- With US
ggplot(subset(train_data, country_destination != "NDF" & signup_method == "facebook"), aes(x = signup_method)) +
  geom_bar()+
  facet_wrap(~country_destination)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 9. Sign_up Flow
unique(train_data$signup_flow)
summary(train_data$signup_flow)
count(train_data$signup_flow, vars = NULL)

# 10. Language
unique(train_data$language)
summary(train_data$language)

# language across the users that did not ever make a reservation through Airbnb
train_data <- within(train_data, language <- factor(language, 
                                                    levels=names(sort(table(language),
                                                                      decreasing=TRUE))))
# With English
wEng <- ggplot(subset(train_data, country_destination == "NDF"), 
               aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency distribution - NDF, All Languages")

# Without English
woEng <- ggplot(subset(train_data, country_destination == "NDF" & language != "en"), 
                aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency Distribution - NDF, W/o English")

grid.arrange(wEng, woEng, ncol = 1)

# language across the users who made a reservation through Airbnb

# With English
wEng <- ggplot(subset(train_data, country_destination != "NDF"), 
               aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency distribution - Reservations, All Languages")

# Without English

woEng <- ggplot(subset(train_data, country_destination != "NDF" & language != "en"), 
                aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency Distribution - Reservations, W/o English")

grid.arrange(wEng, woEng, ncol = 1)

# 11. SIGNUP_APP

# unique value for signup_app are : Android iOS Moweb Web
unique(train_data$signup_app)

#frequency plot of signup app 
qplot(x = signup_app, data = train_data)

#frequenct plot for each destination country of signup app
ggplot(subset(train_data, country_destination != "NDF"), aes(x=signup_app)) +
  geom_bar()+
  facet_wrap(~country_destination, scales = "free")


#12 .FIRST_DEVICE_TYPE

# unique value for first device type are : Android Phone Android Tablet Desktop (Other) iPad iPhone Mac Desktop Windows Desktop
unique(train_data$first_device_type)

#frequency plot of first_device_type 
qplot(x = first_device_type, data = train_data)

#frequenct plot for each destination country of first_device_type
ggplot(subset(train_data, country_destination != "NDF"), aes(x=first_device_type)) +
  geom_bar()+
  facet_wrap(~country_destination, scales = "free")


#13.FIRST_BROWSER

# unique value for first_browser
unique(train_data$first_browser)

#frequency plot of first_browser 
qplot(x = first_browser, data = train_data)

#frequenct plot for each destination country of first_browser
ggplot(subset(train_data, country_destination != "NDF"), aes(x=first_browser)) +
  geom_bar()+
  facet_wrap(~country_destination, scales = "free")


#14. COUNTRY_DESTINATION

# unique value for country_destination
unique(train_data$country_destination)

#frequency plot of country_destination
qplot(x = country_destination, data = train_data)

#frequenct plot for each destination country of signup app
ggplot(subset(train_data, country_destination != "NDF"), aes(x=country_destination)) +
  geom_bar()+
  facet_wrap(~country_destination, scales = "free")



#dataset for each class is not entirely balanced.


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

# Making age > 1996 and < 1906 as NA
# Making age >110 as NA
cleaned_train_data$age[cleaned_train_data$age >= 1996] <- NA
cleaned_train_data$age[cleaned_train_data$age <= 1906 & cleaned_train_data$age >= 110] <- NA
summary(cleaned_train_data$age)


#calculating age for data having current year as value in age 
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

#Let's assume that age values less than 18 are faulty input 
cleaned_train_data$age[cleaned_train_data$age < 18] <- NA
summary(cleaned_train_data$age)


# 3. Cleaning gender feature
#--------------------------------------------------
#gender: convert unknown to NA so that R can automatically process it. 
cleaned_train_data$gender[cleaned_train_data$gender == "-unknown-"] <- NA
summary(cleaned_train_data$gender)



#cleaned_train_data$month_first_book <- factor(format(cleaned_train_data$date_first_booking, format = "%B"), 
#                                      levels = c("January", "February", "March", "April", 
#                                                 "May", "June", "July", "August", "September",
#                                                 "October", "November", "December"))
#summary(cleaned_train_data$month_first_book) 

#cleaned_train_data <- subset(cleaned_train_data, select= -month_first_book)
#summary(cleaned_train_data)


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

#average time for each action_type
#(actiontime <- sessions %>% group_by(action_detail) %>% summarize(mean = mean(na.omit(secs_elapsed))))


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


#for deleting column X
#merged_df <- merged_df[,-c(1)]


# 8. Near Zero Variance
#-------------------------------------------------------------------


#identify variables with near or zero variance
nzv <- nearZeroVar(merged_df, saveMetrics= TRUE) #saving metrics to view variable details
nzv
#nzv[nzv$nzv,]

dim(merged_df) #342 variables to start

nzv <- nearZeroVar(merged_df) #no metrics just return identified columns indexes
nzv

#-------------------------------
# Data Exploration
#--------------------------------
# 1. Age
# To see if there exists any outliers 
boxplot(train_data$age)
#To check if outliers exist in all the gender cases.
qplot(x = gender, y = age, data = train_data, 
      geom = "boxplot") 
#Age by destination country
ggplot(train_data, aes(country_destination, age)) +
  geom_boxplot(aes()) + ylim(25,50)

ggplot(train_data, aes(age)) +
  geom_histogram(binwidth = 1, color = 'black', fill = '#099DD9') +
  geom_histogram(data=subset(train_data,age==20), color = "black", fill="red", binwidth = 1) +
  scale_x_continuous(limits = c(15, 25), breaks = seq(15, 25, 1)) +
  facet_wrap(~country_destination, ncol = 3, scales = "free")

# 2. Gender
ggplot(train_data, aes(x = gender)) + geom_bar()

# 3. Date first Booking
# Converting to Date Format
train_data$date_first_booking <- as.character(train_data$date_first_booking)
# copying the values of date_account_created to the column with date_first_booking
train_data$date_account_created <- as.character(train_data$date_account_created)
train_data$date_first_booking <- ifelse(train_data$date_first_booking == "", train_data$date_account_created, train_data$date_first_booking)
train_data$date_account_created <- as.Date(train_data$date_account_created)
train_data$date_first_booking <- as.Date(train_data$date_first_booking)    

summary(train_data$date_account_created)
#Considering Month (We create a new column)
train_data$month_first_book <- factor(format(train_data$month_first_book, format = "%B"), 
                                      levels = c("January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September",
                                                 "October", "November", "December"))
summary(train_data$month_first_book)
qplot(x =month_first_book, data = train_data)



# 8. Signup_Method
unique(train_data$signup_method)
summary(train_data$signup_method)
count(train_data$signup_method, vars = NULL)

#frequency plot of signup_method
qplot(x = signup_method, data = train_data)

# frequenct plot for each destination country of signup_method- Without US
ggplot(subset(train_data, country_destination != "NDF" & country_destination != "US"  & signup_method == "facebook"), aes(x = signup_method)) +
  geom_bar()+
  facet_wrap(~country_destination)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# #frequenct plot for each destination country of signup_method- With US
ggplot(subset(train_data, country_destination != "NDF" & signup_method == "facebook"), aes(x = signup_method)) +
  geom_bar()+
  facet_wrap(~country_destination)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 9. Sign_up Flow
unique(train_data$signup_flow)
summary(train_data$signup_flow)
count(train_data$signup_flow, vars = NULL)

# 10. Language
unique(train_data$language)
summary(train_data$language)

# language across the users that did not ever make a reservation through Airbnb
train_data <- within(train_data, language <- factor(language, 
                                                    levels=names(sort(table(language),
                                                                      decreasing=TRUE))))
# With English
wEng <- ggplot(subset(train_data, country_destination == "NDF"), 
               aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency distribution - NDF, All Languages")

# Without English
woEng <- ggplot(subset(train_data, country_destination == "NDF" & language != "en"), 
                aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency Distribution - NDF, W/o English")

grid.arrange(wEng, woEng, ncol = 1)

# language across the users who made a reservation through Airbnb

# With English
wEng <- ggplot(subset(train_data, country_destination != "NDF"), 
               aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency distribution - Reservations, All Languages")

# Without English

woEng <- ggplot(subset(train_data, country_destination != "NDF" & language != "en"), 
                aes(language)) +
  geom_bar(aes(fill = gender)) +
  ggtitle("Frequency Distribution - Reservations, W/o English")

grid.arrange(wEng, woEng, ncol = 1)


#remove or filter columns with nearZeroVar
filteredDescr <- merged_df[, -nzv]
dim(filteredDescr) #297 columns remaining

merged_df <- filteredDescr


dim(merged_df)  #34342    22


df.info(merged_df)

summary(merged_df)


# 9. Dealing with Unknown and Blank values
#-------------------------------------------------------------------

# a. first_affiliate_tracked  # convert blank to unknown

table(merged_df$first_affiliate_tracked) 
levels(merged_df$first_affiliate_tracked)[levels(merged_df$first_affiliate_tracked) == ""] <- "Unknown"


# b. first_browser

table(merged_df$first_browser)   # change name of -unknown-

#levels(merged_df$first_browser)[levels(merged_df$first_browser) == "-unknown-"] <- "Unknown"
#merged_df$first_browser[merged_df$first_browser == "-unknown-"] <- "Unknown"


# c. freqAction_detail
table(merged_df$freqAction_detail)  #  blank to -unknown-
merged_df$freqAction_detail[merged_df$freqAction_detail == ""] <- "Unknown"


str(merged_df)


# 10. Writing out merged & cleaned airbnb training data into CSV file
#-------------------------------------------------------------------

#writing out cleaned dataframe into csv file
write.csv(merged_df, file= "Airbnb_cleaned.csv")



airbnb_cleaned_train_df<- read.csv("Airbnb_cleaned.csv")
saveRDS(airbnb_cleaned_train_df, "airbnb_cleaned_train_df.rds")




# 11. Converting datatype of some features in the dataset
#-------------------------------------------------------------------

#id -> int
#airbnb_cleaned_train_df$id <- as.character(airbnb_cleaned_train_df$id)

#date_account_created ->date
#airbnb_cleaned_train_df$date_account_created <- as.character(airbnb_cleaned_train_df$date_account_created) 



df.info(airbnb_cleaned_train_df)

airbnb_model_train_df <- airbnb_cleaned_train_df[,-c(1,2,3,5)]

str(airbnb_model_train_df)

#---------------------------
# Step 5: Model Building
#---------------------------


# 1. Creating dummy variables for features with datatype as Factor
#----------------------------------------------------------------------

#gender,signup_method,language,affiliate_channel, affiliate_provider, first_Affiliate_tracked,signup_app,
#first_device_type,first_browser,country_destination, freqAction_Detail, device

head(airbnb_model_train_df)

# Encode categorical values

#lets relevel the Default factor so Yes becomes the Positive class by default (Sensitivity)
airbnb_model_train_df$country_destination <- relevel(airbnb_model_train_df$country_destination, ref="US")

airbnb.dmodel <- dummyVars( ~ ., data=airbnb_model_train_df, fullRank=T)
airbnb.d <- as.data.frame(predict(airbnb.dmodel, airbnb_model_train_df))
saveRDS(airbnb.d ,"airbnb_dummyvars.rds")


airbnb.d$country_destination.US <- airbnb_model_train_df$country_destination #copy back DV

str(airbnb.d)
str(airbnb.dmodel)
str(airbnb_model_train_df)

# 2. Cross Validation
#------------------------------------------------------------

#setup control function for resampling and multiclass classification performance
#using 10 fold cross validation
ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,
                     classProbs=T, savePredictions=T) #saving predictions from each resample fold




# 3. Parallel Processing
#------------------------------------------------------------
#Register core backend, using 8 cores
cl <- makeCluster(10)
registerDoParallel(cl)

#list number of workers
getDoParWorkers()




# A. LINEAR MODEL
#--------------------------------


# 1.logistic regression
#--------------------------------
set.seed(44)

?train

airbnb.glm <-  train(country_destination ~ ., data=airbnb_model_train_df, method="glm", family="multinomial", metric="logLoss", trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.glm, "airbnb.glm.rds")

summary(airbnb.glm)

?Multinomial

varImp(airbnb.glm)
getTrainPerf(airbnb.glm)

#calculate resampled accuracy/confusion matrix using extracted predictions from resampling
confusionMatrix(airbnb.glm$pred$pred, airbnb.glm$pred$obs) #take averages



# 2.linear discriminant analysis
#--------------------------------
set.seed(44)
airbnb.lda <-  train(country_destination ~ ., data=airbnb_model_train_df, method="lda", metric="ROC", trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.lda ,"airbnb.lda.rds")

summary(airbnb.lda)
varImp(airbnb.lda)
confusionMatrix(airbnb.lda$pred$pred, airbnb.lda$pred$obs) #take averages


# 3. quadratic distriminant analysis
#--------------------------------
set.seed(44)
airbnb.qda <-  train(country_destination ~ ., data=airbnb_model_train_df, method="qda", metric="ROC", trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.qda , "airbnb.qda.rds")
summary(airbnb.qda)
getTrainPerf(airbnb.qda)



# 4. k nearest neighbors classification
#--------------------------------
set.seed(44) 
airbnb.knn <-  train(country_destination ~ ., data=airbnb_model_train_df, method="knn", metric="ROC", trControl=ctrl, tuneLength=10,allowParallel = TRUE) #let caret decide 10 best parameters to search
saveRDS(airbnb.knn, "airbnb.knn.rds")

summary(airbnb.knn)
plot(airbnb.knn)
getTrainPerf(airbnb.knn)

confusionMatrix(airbnb.knn$pred$pred, airbnb.knn$pred$obs) #make sure to select resamples only for optimal parameter of K



# B. NON-LINEAR MODEL
#--------------------------------

# 1. DECISION TREE
#-------------------
set.seed(44)
airbnb.dt <- train(country_destination ~ ., data=airbnb_model_train_df, method="rpart",tuneLength=4,trControl=ctrl,allowParallel = TRUE)
saveRDS(airbnb.dt, "airbnb.dt.rds")

summary(airbnb.dt)
plot(airbnb.dt,main='Decision Tree')

plot(density(resid(airbnb.dt))) #A density plot
qqnorm(resid(airbnb.dt)) # A quantile normal plot - good for checking normality
qqline(resid(airbnb.dt))

p.airbnb.dt <- predict(airbnb.dt, newdata = airbnb.test)
R.airbnb.test.dt <- postResample(p.airbnb.dt, airbnb.test$country_destination)
R.airbnb.test.dt


#using rpart for regression tree
library(rpart) #faster than tree
install.packages("tree")
library(tree) #has useful functions to use with rpart

#create tree
airbnb.dt2 <- rpart(airbnb_cleaned_train_df ~ ., data=airbnb_model_train_df,control=ctrl,allowParallel = TRUE)

#summarize full tree (no pruning)
airbnb.dt2

#by default tree plot needs some adjustments and labeling
plot(airbnb.dt2)
text(airbnb.dt2, pretty=0)

#rather than using default lets use new library
install.packages("rpart.plot")
library(rpart.plot)

#very readable defaults
rpart.plot(airbnb.dt2)


# 2. BAGGING TREE
#-------------------
set.seed(44)
airbnb.bt <- train(country_destination ~ ., data=airbnb_model_train_df, method="treebag",tuneLength=4, trControl=ctrl)
airbnb.bt

p.airbnb.bt <- predict(airbnb.bt, newdata = airbnb.test)
R.airbnb.test.bt <- postResample(p.airbnb.bt, airbnb.test$country_destination)
R.airbnb.test.bt

qqnorm((airbnb.test$country_destination - p.airbnb.bt)) 
qqline((p.airbnb.bt))

# 3. RANDOM FOREST
#-------------------

cores <- 10 
cl <- makeForkCluster(cores) 
clusterSetRNGStream(cl, 489)
registerDoParallel(cl)

set.seed(489)
airbnb.rf <- train(country_destination ~ ., data=airbnb_model_train_df, method="rf",tuneLength=4,trControl=ctrl)
airbnb.rf
plot(airbnb.rf,main='Random Forest')

p.airbnb.rf <- predict(airbnb.rf, newdata = airbnb.test)
R.airbnb.test.rf <- postResample(p.airbnb.rf, airbnb.test$country_destination)
R.airbnb.test.rf

qqnorm((airbnb.test$country_destination - p.airbnb.rf)) 
qqline((p.airbnb.rf))


# 4. BOOSTING
#-------------------
set.seed(44)
airbnb.boost <- train(country_destination ~ ., data=airbnb_model_train_df, method="gbm",tuneLength=4,trControl=ctrl)
airbnb.boost
getTrainPerf(airbnb.boost)
plot(airbnb.boost,main="Boosting")

p.airbnb.boost <- predict(airbnb.boost, newdata = airbnb.test)
R.airbnb.test.boost <- postResample(p.airbnb.boost, airbnb.test$country_destination)
R.airbnb.test.boost

qqnorm((airbnb.test$country_destination - p.airbnb.boost)) 
qqline((p.airbnb.boost))



# 5. Neural Network
#-------------------
modelLookup("nnet")
m.nnet <- train(country_destination~ ., 
                trControl = ctrl,
                metric = "ROC", #using AUC to find best performing parameters
                preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                data = airbnb_model_train_df, 
                method = "nnet")
m.nnet
plot(m.nnet)
p.nnet <- predict(m.nnet,credit.dummy.test)
confusionMatrix(p.nnet,credit.dummy.test$country_destination)



# 6. SVM
#-------------------
##svm with radial kernel
modelLookup("svmRadial")
m.svm <- train(country_destination~ ., 
               trControl = ctrl,
               metric = "ROC", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = airbnb_model_train_df, 
               method = "svmRadial")
m.svm
plot(m.svm)
p.svm<- predict(m.svm,credit.dummy.test)
confusionMatrix(p.svm,credit.dummy.test$country_destination)




#Closing cluster (releasing computers cores)
stopCluster(cl)


#---------------------------
# Step 6: Model Evaluation
#---------------------------

# 1. Linear Models
#---------------------
models.linear<- list("logistic"=airbnb.lm, "Ridge" = airbnb.ridge, "Lasso"=airbnb.lasso, "PCR(kernelpls)" = airbnb.pcr1, "PCR(pls)" = airbnb.pcr2, "PCR(pcr)" = airbnb.pcr3)

# 2. Non-Linear Models
#---------------------
models.nonlinear<- list("DecisionTree"=airbnb.dt, "Bagging" = airbnb.bt, "RandomForest"=airbnb.rf, "Boosting"=airbnb.boost, "GAM"=airbnb.gam)


# Result matrix: LINEAR
airbnb.resamples.linear<- resamples(models.linear)
summary(airbnb.resamples.linear)

# Plot model performances
bwplot(airbnb.resamples.linear, metric="MAE")
bwplot(airbnb.resamples.linear, metric="RMSE")
bwplot(airbnb.resamples.linear, metric="Rsquared")


# Result matrix: NON-LINEAR
airbnb.resamples.nonlinear<- resamples(models.nonlinear)
summary(airbnb.resamples)

# Plot model performances
bwplot(airbnb.resamples.nonlinear, metric="MAE")
bwplot(airbnb.resamples.nonlinear, metric="RMSE")
bwplot(airbnb.resamples.nonlinear, metric="Rsquared")


# MODEL Performances- ALL MODELS COMBINED
models.all<- list("Ridge" = airbnb.ridge, "Lasso"=airbnb.lasso, "PCR(kernelpls)" = airbnb.pcr1, "PCR(pls)" = airbnb.pcr2, "PCR(pcr)" = airbnb.pcr3, "DecisionTree"=airbnb.dt, "Bagging" = airbnb.bt, "RandomForest"=airbnb.rf, "Boosting"=airbnb.boost, "GAM"=airbnb.gam)

airbnb.resamples.all<- resamples(models.all)
summary(airbnb.resamples.all)

bwplot(airbnb.resamples.all, metric="ROC")
bwplot(airbnb.resamples.all, metric="Sens")
bwplot(airbnb.resamples.all, metric="Spec")





#---------------Additional code -----------

# 9. Correlation
#-------------------------------------------------------------------

#first create correlation matrix of variables
descrCor <-  cor(filteredDescr)

#summarize the correlations some very high correlations
summary(descrCor[upper.tri(descrCor)])

#identified variables with correlation .75 or higher
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
highlyCorDescr

#filter out these variables from dataset
filteredDescr <- filteredDescr[,-highlyCorDescr]

#creaet new correlation matrix to verify
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)]) #no correlations greater than .75




