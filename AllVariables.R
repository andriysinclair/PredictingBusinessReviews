
# Libraries ---------------------------------------------------------------


library(jsonlite)
library(stringr)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(nnet)
library(glmnet)
library(tidyr)
library(randomForest)
library(tidytext)
library(modelsummary)


# Loading datasets --------------------------------------------------------


user_data <- load("yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
review_data <- load("yelp_review_small.Rda")
#user_data_small <- stream_in(file("yelp_academic_dataset_user.json"))


# Cleaning user data ------------------------------------------------------

usdup <- sum(duplicated(user_data_small$user_id))
usdup      #all users appear once

print(sum(is.na(user_data_small))) # there are no missing values !



print(round(colSums(user_data_small=="")/nrow(user_data_small)*100,2))
# over 95% of values in elite are empty

  
# changing "elite" variables to report the amount of times a user recieved "elite"
# status
  
user_data_small$years_elite <- ifelse(user_data_small$elite=="",0,
                                    str_count(user_data_small$elite, ",") + 1)


# converting "yelping since" to data time
user_data_small$yelping_since <- as.POSIXct(user_data_small$yelping_since, format="%Y-%m-%d %H:%M:%S")

# We can use this to find the amount of days a user has been yelping
# Which could offer some explanatory power
user_data_small$days_yelping <- as.numeric(difftime(Max(user_data_small$yelping_since), user_data_small$yelping_since, units = "days"))







#coding all "none" values as 0, and adding a friend count

user_data_small$friends <- ifelse(user_data_small$friends=="None",0,
                                  str_count(user_data_small$friends, ",") + 1)

# Checking for negative values
print(round(colSums(user_data_small<0)/nrow(user_data_small)*100,2))
# No negative values in relevant section

# Checking for outliers, outliers defined as those being above or below 3sd
# from the mean


# subsetting only relevant variables
user_data_clean <- user_data_small %>%
  select(-c("yelping_since", "name"))

# dropping 84 na values

user_data_clean <- na.omit(user_data_clean)


# Cleaning business data --------------------------------------------------

busdup <- sum(duplicated(business_data$business_id))
busdup     # all business appear once
print( round( colSums(is.na(business_data))/nrow(business_data)*100,2 ) )
# Variables with missing values will not be included

table(business_data$attributes$BusinessAcceptsCreditCards)
# only 6025 False values, if I include all rows without missing values
# dataset would not be representative

# subsetting all relevant variables
business_data_clean <- business_data %>%
  select(c("business_id", "state", "stars", "review_count", "is_open", "name"))

states_count <- table(business_data_clean$state)
states_to_remove <- names(states_count[states_count < 10])
business_data_clean <- business_data_clean[!(business_data_clean$state %in% states_to_remove), ]

# making "state" a factor variable

business_data_clean$state <- factor(business_data_clean$state)





# Cleaning Review data ----------------------------------------------------

revdup_bus <- sum(duplicated(review_data_small$business_id))
revdup_bus   # 1,261,472 businesses are review more than once

revdup_user <- sum(duplicated(review_data_small$user_id))
revdup_user   # 686,446 users leave more than one review

print(sum(is.na(review_data_small))) # there are no missing values!


# extracting year, month and weekday from "date" variable, then dropping "date" variable

review_data_small$date <- as.POSIXct(review_data_small$date, format="%Y-%m-%d %H:%M:%S")

review_data_small$review_month <- format(review_data_small$date, "%m")
review_data_small$review_year <- format(review_data_small$date, "%Y")
review_data_small$review_day <- weekdays(review_data_small$date)
## Introduces 1136 missing values
review_data_small <- na.omit(review_data_small)

review_data_small$date <- NULL

# turning "review_month","review_year" and "review_day" into factors

review_data_small$review_month <- factor(review_data_small$review_month)
review_data_small$review_year <- factor(review_data_small$review_year)
review_data_small$review_day <- factor(review_data_small$review_day)

# Merging Data ------------------------------------------------------------

merged_df <- merge(merge(review_data_small, user_data_clean, by = "user_id"), business_data_clean, by = "business_id")



# allocating informative names

merged_df <- merged_df %>% rename(review_stars = stars.x, review_useful = useful.x, 
                                  review_cool = cool.x, review_funny = funny.x) %>%
  rename(user_review_count = review_count.x, user_useful = useful.y, user_funny = funny.y,
         user_cool = cool.y) %>%
  rename(business_stars = stars.y, business_review_count = review_count.y)


# Duplicated reviews, remove
merged_df <- merged_df[!duplicated(merged_df$text),]

# "name" and "text" variables have been kept for reference

#write.csv(merged_df, file = "merged_df1.csv", row.names = FALSE)



# Test vs train -----------------------------------------------------------

merged_df1 <- merged_df %>% select(-c("business_id","user_id","review_id","elite"))

# testing data types
for (i in colnames(merged_df1)) {
  print(class(merged_df1[[i]]))
}

## Owing to constraints in computational resources, only 40,000 observations will be used

set.seed(1)

# This dataset will be used for SA
small_df <- merged_df1[sample(40000),]


# "name" and "text" are not needed now
small_df_reg <- small_df %>% select(-c("name", "text"))

train_split <- sample(1:nrow(small_df_reg), 3*nrow(small_df_reg)/4)

# dropping irrelevant variables
train <- small_df_reg[train_split,] 
test <- small_df_reg[(-train_split),] 

trainX <- subset(train,select = -review_stars)
trainY <- subset(train, select = review_stars)
testX <- subset(test, select = -review_stars)
testY <- subset(test, select = review_stars)



# Random choice benchmark -------------------------------------------------


randY <- sample(1:5, length(testY$review_stars), replace = TRUE)

#random choice MSE

rand_MSE<-mean((randY-testY$review_stars)^2)
rand_MSE

#random choice classification accuracy



randcount <- sum(randY==testY)
randcount

randclass_acc <- randcount/length(testY$review_stars)

randclass_acc     # as expected



# Multinomial logit model -------------------------------------------------

#fitting multinomial logit model due to discreet outcome variables

ml_model <- multinom(review_stars ~., data=train)


# predictions


mlmod_predict <- predict(ml_model, newdata = testX)
#28 missing values



#multinom MSE


ml_MSE_reg<-mean((as.numeric(mlmod_predict)-testY$review_stars)^2)

ml_MSE_reg


#classification accuracy


mlcount <- sum(as.numeric(mlmod_predict)==testY, na.rm=TRUE)
mlcount

mlclass_acc_reg <- mlcount/length(testY$review_stars)

mlclass_acc_reg


# Linear Model ------------------------------------------------------------

#fitting a linear model

linmod <- lm(review_stars ~., data=train)

#LM predictions
linmod_predict <- predict(linmod, newdata = testX)
# 28 missing values

#LM MSE calculations


lm_MSE_reg<-mean((linmod_predict-testY$review_stars)^2, na.rm=TRUE)

lm_MSE_reg



# Shrinkage Methods -------------------------------------------------------

# Transforming values into a matrix

trainX_num <- model.matrix(~. -1, data = trainX)
trainY_num <- model.matrix(~. -1, data = trainY)
testX_num <- model.matrix(~. -1, data = testX)
testY_num <- model.matrix(~. -1, data = testY)




####### RIDGE ###########

# Cross - validation to find best lambda



cv.out = cv.glmnet(trainX_num, trainY_num, alpha = 0, nfolds = 10) 
bestlam = cv.out$lambda.min  


plot(cv.out) + 
  title("")

ridge.mod<-glmnet(trainX_num, trainY_num, alpha = 0, lambda = bestlam, thresh = 1e-12)
coef(ridge.mod) #coefficients
ridge_pred = predict(ridge.mod, s = bestlam, newx = testX_num) # Use best lambda to predict test data
ridgeMSE_reg <- mean((ridge_pred - testY$review_stars)^2) # Calculate test MSE


####### LASSO #############

cv.out1 = cv.glmnet(trainX_num, trainY_num, alpha = 1, nfolds = 10) 
bestlam1 = cv.out1$lambda.min  


plot(cv.out1) + 
  title("")


Prediction


lasso.mod<-glmnet(trainX_num, trainY_num, alpha = 1, lambda = bestlam1, thresh = 1e-12)
lasso_pred = predict(lasso.mod, s = bestlam1, newx = testX_num) # Use best lambda to predict test data
lassoMSE_reg <- mean((lasso_pred - testY$review_stars)^2) # Calculate test MSE



######## Random Forest ##########

rf <- randomForest(review_stars~., data=train, proximity=FALSE, ntrees = 100)

rf_predict <- predict(rf, newdata = testX)

# RF MSE

rf_MSE_reg <- mean((rf_predict) - testY$review_stars^2) # Calculate test MSE
rf_MSE_reg
# poor

## Turning into a classification problem

train$review_stars <- factor(train$review_stars)

crf <- randomForest(review_stars~., data=train, proximity=FALSE, ntree =100)
crf_predict <- predict(crf, newdata = testX)

rfcount <- sum(as.numeric(crf_predict)==testY)
rfclass_acc <- rfcount/length(testY$review_stars)
rfclass_acc