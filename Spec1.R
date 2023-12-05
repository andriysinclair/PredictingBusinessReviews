
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
library(cowplot)
library(gridGraphics)
library(patchwork)


# Loading datasets --------------------------------------------------------


user_data <- load("yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
review_data <- load("yelp_review_small.Rda")



# Cleaning user data ------------------------------------------------------

print(sum(duplicated(user_data_small$user_id)))
#all users appear once

print(sum(is.na(user_data_small))) # there are no missing values !

  
# changing "elite" variables to report the amount of times
#a user recieved "elite"
# status
  
user_data_small$years_elite <- ifelse(user_data_small$elite=="",0,
                                    str_count(user_data_small$elite, ",") + 1)


# converting "yelping since" to data time
user_data_small$yelping_since <- as.POSIXct(user_data_small$yelping_since, 
                                            format="%Y-%m-%d %H:%M:%S")

# We can use this to find the amount of days a user has been yelping
# Which could offer some explanatory power
user_data_small$days_yelping <- as.numeric(difftime(Max(user_data_small$yelping_since),
                                                    user_data_small$yelping_since, units = "days"))


#coding all "none" values as 0, and adding a friend count

user_data_small$friends <- ifelse(user_data_small$friends=="None",0,
                                  str_count(user_data_small$friends, ",") + 1)

# Checking for negative values
print(round(colSums(user_data_small<0)/nrow(user_data_small)*100,2))
# No negative values in relevant section


# subsetting only relevant variables
user_data_clean <- user_data_small %>%
  select(-c("yelping_since", "name"))

# dropping 84 na values

user_data_clean <- na.omit(user_data_clean)


# Cleaning business data --------------------------------------------------

print(sum(duplicated(business_data$business_id)))
# all business appear once
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

print(sum(duplicated(review_data_small$business_id)))
 # 1,261,472 businesses are reviewed more than once

print(sum(duplicated(review_data_small$user_id)))
# 686,446 users leave more than one review

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

merged_df <- merge(merge(review_data_small, user_data_clean, by = "user_id"), 
                   business_data_clean, by = "business_id")



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

small_df_reg$compliment_cool <- NULL

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

# Linear Model ------------------------------------------------------------

#fitting a linear model

linmod1 <- lm(review_stars ~., data=train)

alias(linmod1)
# "compliment_cool" is collinear with other variables, so will be dropped 
# and model re-run

#LM predictions
linmod_predict1 <- predict(linmod1, newdata = testX)

#LM MSE calculations
lm_MSE1<-mean((linmod_predict1-testY$review_stars)^2)
lm_MSE1

# Evaluating training data MSE

linmod_predict1b <- predict(linmod1, newdata = trainX)

lm_MSE1b<-mean((linmod_predict1b-trainY$review_stars)^2)
lm_MSE1b


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

ridge.mod1<-glmnet(trainX_num, trainY_num, alpha = 0, lambda = bestlam, thresh = 1e-12)
coef(ridge.mod1) #coefficients
ridge_pred1 = predict(ridge.mod1, s = bestlam, newx = testX_num) # Use best lambda to predict test data
ridgeMSE1 <- mean((ridge_pred1 - testY$review_stars)^2) # Calculate test MSE
ridgeMSE1

# Evaluating training data MSE

ridge_pred1b = predict(ridge.mod1, s = bestlam, newx = trainX_num)

ridge_MSE1b<-mean((ridge_pred1b-trainY$review_stars)^2)
ridge_MSE1b

####### LASSO #############

cv.out1 = cv.glmnet(trainX_num, trainY_num, alpha = 1, nfolds = 10) 
bestlam1 = cv.out1$lambda.min  

lasso.mod1<-glmnet(trainX_num, trainY_num, alpha = 1, lambda = bestlam1, thresh = 1e-12)
lasso_pred1 = predict(lasso.mod1, s = bestlam1, newx = testX_num) # Use best lambda to predict test data
lassoMSE1 <- mean((lasso_pred1 - testY$review_stars)^2) # Calculate test MSE

# Evaluating training data MSE

lasso_pred1b = predict(lasso.mod1, s = bestlam1, newx = trainX_num)

lasso_MSE1b<-mean((lasso_pred1b-trainY$review_stars)^2)
lasso_MSE1b


knitr::stitch('AllVariables.R')
