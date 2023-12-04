View(merged_df)


# Sentiment analysis score ------------------------------------------------

textt <- small_df$text
text_df <- tibble(line = 1:length(textt),text=textt)
token_df <- text_df %>%
  unnest_tokens(word, text) 

# Removing stop words
#data(stop_words)
result_df <- left_join(text_df, token_df, by = "line")
#result_df <- result_df %>%
#  anti_join(stop_words)

# Most common words plot
# Might need to remove stop words just for this part

result_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 20000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

afinn_scores <- get_sentiments("afinn")

result_df <- left_join(result_df, afinn_scores, by = "word")
result_df$value <- replace(result_df$value, is.na(result_df$value), 0)

# Summarize sentiment scores for each sentence
sentiment_summary <- result_df %>%
  group_by(text) %>%
  summarize(sentiment_score = sum(value)) %>%
  arrange(match(text, result_df$text))

sentiment_summary1 <- result_df %>%
  group_by(text) %>%
  summarize(
    sentiment_score = sum(value, na.rm = TRUE),
    num_non_zero = sum(value != 0),
    scaled_ss = ifelse(num_non_zero>0, sentiment_score/(num_non_zero*5), 0)
  ) %>% arrange(match(text, result_df$text))

small_df_reg$ss <- sentiment_summary1$scaled_ss

small_df_reg$is_open <- factor(small_df_reg$is_open)

set.seed(1)
train_split <- sample(1:nrow(small_df_reg), 3*nrow(small_df_reg)/4)

# dropping irrelevant variables
train <- small_df_reg[train_split,] 
test <- small_df_reg[(-train_split),] 

trainX <- subset(train,select = -review_stars)
trainY <- subset(train, select = review_stars)
testX <- subset(test, select = -review_stars)
testY <- subset(test, select = review_stars)

# Checking column types
for (i in colnames(train)) {
  print(class(train[[i]]))
}


# Random choice model -----------------------------------------------------

randY <- sample(1:5, length(testY$review_stars), replace = TRUE)
#random choice MSE
rand_MSE<-mean((randY-testY$review_stars)^2)
rand_MSE
#random choice classification accuracy
randcount <- sum(randY==testY)
randcount
randclass_acc <- randcount/length(testY$review_stars)
randclass_acc     # as expected


# OLS ---------------------------------------------------------------------

linmod <- lm(review_stars ~., data=train)

#LM predictions
linmod_predict <- predict(linmod, newdata = testX)

#LM MSE calculations

lm_MSE_sa<-mean((linmod_predict-testY$review_stars)^2)

lm_MSE_sa

lm_train_predict_sa <- predict(linmod, newdata = trainX)
lm_train_MSE_sa <- mean((train_predict-trainY$review_stars)^2)
lm_train_MSE_sa

# Shrinkage methods -------------------------------------------------------



trainX_num <- model.matrix(~. -1, data = trainX)
trainY_num <- model.matrix(~. -1, data = trainY)
testX_num <- model.matrix(~. -1, data = testX)
testY_num <- model.matrix(~. -1, data = testY)


####### RIDGE ###########

# Cross - validation to find best lambda

cv.out = cv.glmnet(trainX_num, trainY_num, alpha = 0, nfolds = 10) 
bestlam = cv.out$lambda.min  


ridge.mod<-glmnet(trainX_num, trainY_num, alpha = 0, lambda = bestlam, thresh = 1e-12)
coef(ridge.mod) #coefficients
ridge_pred = predict(ridge.mod, s = bestlam, newx = testX_num) # Use best lambda to predict test data
ridgeMSE_sa <- mean((ridge_pred - testY$review_stars)^2) # Calculate test MSE
ridgeMSE_sa




######### Lasso ##########

cv.out1 = cv.glmnet(trainX_num, trainY_num, alpha = 1, nfolds = 10)  
bestlam1 = cv.out1$lambda.min  


plot(cv.out1) + 
  title("")





lasso.mod<-glmnet(trainX_num, trainY_num, alpha = 1, lambda = bestlam1, thresh = 1e-12)
lasso_pred = predict(lasso.mod, s = bestlam1, newx = testX_num) # Use best lambda to predict test data
lassoMSE_sa <- mean((lasso_pred - testY$review_stars)^2) # Calculate test MSE


# Random Forest -----------------------------------------------------------



rf <- randomForest(review_stars~., data=train, proximity=FALSE, ntree =100)
rf_predict <- predict(rf, newdata = testX)

# RF MSE

rf_MSE <- mean((rf_predict) - testY$review_stars^2) # Calculate test MSE
rf_MSE
# poor

## Turning into a classification problem

train$review_stars <- factor(train$review_stars)

crf <- randomForest(review_stars~., data=train, proximity=FALSE, ntree =100)
crf_predict <- predict(crf, newdata = testX)

rfcount <- sum(as.numeric(crf_predict)==testY)
rfclass_acc <- rfcount/length(testY$review_stars)
rfclass_acc

# Adaboost ----------------------------------------------------------------

# train a model using our training data
model_adaboost <- boosting(review_stars~., data=train, boos=TRUE, mfinal=50)
summary(model_adaboost)

ab_predict <- predict(model_adaboost, newdata = testX)

# RF classification accuracy

abcount <- sum(as.numeric(ab_predict$class)==testY$review_stars)

abclass_acc <- abcount/length(testY$review_stars)

abclass_acc
