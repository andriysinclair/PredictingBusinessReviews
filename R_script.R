
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


# converting "yelping since to data time"
user_data_small$yelping_since <- as.POSIXct(user_data_small$yelping_since, format="%Y-%m-%d %H:%M:%S")

# Extract the year
# this could pick up monthly variation in reviews if it exists
# it could be possible to get a user's time yelping, but this may be reflected in
# other variables like "review count". But monthly variation in reviews
# will not be picked up elsewhere
user_data_small$user_month_joined <- format(user_data_small$yelping_since, "%m")
user_data_small$user_year_joined <- format(user_data_small$yelping_since, "%Y")
# This introduces 252 missing values, possible because of incorrect formatting 
# 252 values is insignificant so will drop
user_data_small <- na.omit(user_data_small)

#coding all "none" values as 0, and adding a friend count

user_data_small$friends <- ifelse(user_data_small$friends=="None",0,
                                  str_count(user_data_small$friends, ",") + 1)

# Checking for negative values
print(round(colSums(user_data_small<0)/nrow(user_data_small)*100,2))
# No negative values in relevant section

# Checking for outliers, outliers defined as those being above or below 3sd
# from the mean

#converting "user month joined" and "user year joined" to factors
user_data_small$user_month_joined <- factor(user_data_small$user_month_joined)
user_data_small$user_year_joined <- factor(user_data_small$user_year_joined)

# subsetting only relevant variables
user_data_clean <- user_data_small %>%
  select(-c("yelping_since", "name"))


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


# extracting year and month from "date" variable, then dropping "date" variable

review_data_small$date <- as.POSIXct(review_data_small$date, format="%Y-%m-%d %H:%M:%S")

review_data_small$review_month <- format(review_data_small$date, "%m")
review_data_small$review_year <- format(review_data_small$date, "%Y")
## Introduces 568 missing values
review_data_small <- na.omit(review_data_small)

review_data_small$date <- NULL

# turning "review_month" and "review_year" into factors

review_data_small$review_month <- factor(review_data_small$review_month)
review_data_small$review_year <- factor(review_data_small$review_year)

# Merging Data ------------------------------------------------------------

merged_df <- merge(merge(review_data_small, user_data_clean, by = "user_id"), business_data_clean, by = "business_id")



# allocating informative names

merged_df <- merged_df %>% rename(review_stars = stars.x, review_useful = useful.x, 
                                  review_cool = cool.x, review_funny = funny.x) %>%
  rename(user_review_count = review_count.x, user_useful = useful.y, user_funny = funny.y,
         user_cool = cool.y) %>%
  rename(business_stars = stars.y, business_review_count = review_count.y)




# "name" and "text" variables have been kept for reference

write.csv(merged_df, file = "merged_df1.csv", row.names = FALSE)



# Test vs train -----------------------------------------------------------

merged_df1 <- merged_df %>% select(-c("business_id","user_id","review_id","text","name","elite"))

# testing data types
for (i in colnames(merged_df1)) {
  print(class(merged_df1[[i]]))
}

set.seed(1)
train_split <- sample(1:nrow(merged_df1), 3*nrow(merged_df1)/4)

# dropping irrelevant variables
train <- merged_df1[train_split,] 
test <- merged_df1[(-train_split),] 

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


ml_MSE<-mean((as.numeric(mlmod_predict)-testY$review_stars)^2)

ml_MSE


#classification accuracy


mlcount <- sum(as.numeric(mlmod_predict)==testY, na.rm=TRUE)
mlcount

mlclass_acc <- mlcount/length(testY$review_stars)

mlclass_acc


# Linear Model ------------------------------------------------------------

#fitting a linear model

linmod <- lm(review_stars ~., data=train)

#LM predictions
linmod_predict <- predict(linmod, newdata = testX)
# 28 missing values

#LM MSE calculations


lm_MSE<-mean((linmod_predict-testY$review_stars)^2, na.rm=TRUE)

lm_MSE



# Shrinkage Methods -------------------------------------------------------

# normalisation of numeric variables

trainX_norm <- scale(trainX)


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
ridgeMSE <- mean((ridge_pred - testY$review_stars)^2) # Calculate test MSE


####### LASSO #############

cv.out1 = cv.glmnet(as.matrix(trainX), as.matrix(trainY$review_stars), alpha = 1, nfolds = 10) 
bestlam1 = cv.out1$lambda.min  


plot(cv.out1) + 
  title("")


Prediction


lasso.mod<-glmnet(trainX, trainY$review_stars, alpha = 1, lambda = bestlam1, thresh = 1e-12)
lasso_pred = predict(lasso.mod, s = bestlam1, newx = as.matrix(testX)) # Use best lambda to predict test data
mean((lasso_pred - testY$review_stars)^2) # Calculate test MSE



######## Random Forest ##########

rf <- randomForest(review_stars~., data=train, proximity=FALSE)
