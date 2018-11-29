setwd(("/Users/apurvprakash/Desktop/SDSU_MSIS2017/MIS 620/Group Project/Airbnb_Dataset"))


#Loading All Required Libraries
library(ggplot2)
library(plyr)
library(gridBase)
library(grid)
library(gridExtra)
library(lattice)



#----------------------------------------
# Step 1: Loading Airbnb Data
#----------------------------------------

####### Reading Training csv file ##########
train_data<- read.csv("train_users_2.csv")
train_subset <- train_data[1:1000,]
head(train_subset)

head(train_data)
str(train_data)

cleaned_train_data <- train_data[as.integer(substr(train_data$date_account_created,1,4)) >= 2014,]

summary(cleaned_train_data$age)

####### Reading Session csv file ##########
sessions_data<- read.csv("sessions.csv")
head(sessions_data)
str(sessions_data)


####### Reading countries csv file ##########
countries_data<- read.csv("countries.csv")
head(countries_data)
str(countries_data)

####### Reading age_gender_bkts.csv  csv file ##########
age_gender_data<- read.csv("age_gender_bkts.csv")
head(age_gender_data)
str(age_gender_data)

####### Reading sample submission  csv file ##########
sample <- read.csv("sample_submission_NDF.csv")
head(sample)
str(sample)


### Unique Values ####
unique(train_data$gender)
unique(train_data$date_first_booking)
unique(train_data$affiliate_channel)


### cleaning data ### 
head(train_data)

#--------------------------------
# Step 2: Data Exploration 
#--------------------------------
# Check for Null values
colSums(is.na(train_data))

#-------------------------------
# Data Validation:
#--------------------------------
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


#-------------------------------
# Data Cleaning
#--------------------------------

s
#1. Deal with NUll Values

#there are no null values
train_data[is.null(train_data),]

#2. Deal with NA values

train_data[is.na(train_data),]

#3. Deal with NaN values

#there are no NAN values
train_data[is.nan(train_data$age),]


#3. Check balance of class for each countries 


#4. Convert factor to numeric 


#5. Make column values as uniform for each column




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





#---------------------------
# Step 5: Model Building
#---------------------------


library("caret")

# Encode categorical values
airbnb.dmodel <- dummyVars( ~ ., data=train_subset, fullRank=T)
airbnb.d <- as.data.frame(predict(airbnb.dmodel, train_subset))
str(airbnb.d)
str(airbnb.dmodel)


# Settin 5-fold Cross-Validation
ctrl <- trainControl(method = "cv", number=10)




# B. NON-LINEAR REGRESSION MODEL
#--------------------------------

# 1. DECISION TREE
#-------------------
set.seed(44)
airbnb.dt <- train(country_destination ~ ., data=train_subset, method="rpart",tuneLength=4,trControl=ctrl)
airbnb.dt
plot(flight.dt,main='Decision Tree')

plot(density(resid(airbnb.dt))) #A density plot
qqnorm(resid(airbnb.dt)) # A quantile normal plot - good for checking normality
qqline(resid(airbnb.dt))

p.airbnb.dt <- predict(airbnb.dt, newdata = airbnb.test)
R.airbnb.test.dt <- postResample(p.airbnb.dt, airbnb.test$DEPARTURE_DELAY)
R.airbnb.test.dt


#using rpart for regression tree
library(rpart) #faster than tree
install.packages("tree")
library(tree) #has useful functions to use with rpart

#create tree
airbnb.dt2 <- rpart(DEPARTURE_DELAY ~ ., data=flight.train,control=ctrl)

#summarize full tree (no pruning)
flight.dt2

#by default tree plot needs some adjustments and labeling
plot(flight.dt2)
text(flight.dt2, pretty=0)

#rather than using default lets use new library
install.packages("rpart.plot")
library(rpart.plot)

#very readable defaults
rpart.plot(flight.dt2)



