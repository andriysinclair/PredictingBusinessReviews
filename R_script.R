
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


# Loading datasets --------------------------------------------------------


user_data <- load("yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json")) 
review_data <- load("yelp_review_small.Rda")


# Cleaning user data ------------------------------------------------------


View(user_data_small)

# Count the number of zeros in each column
num_zeros_per_column <- round((colSums(user_data_small == 0)/nrow(user_data_small))*100,2)
x_values = names(num_zeros_per_column)
y_values = unname(num_zeros_per_column)
col_sums <- data.frame(variable = x_values, proportion = y_values)
col_sums <- col_sums[col_sums$proportion != 0,]

# creating a plot
zeros_plot <- ggplot(data = col_sums, mapping = aes(x=reorder(variable,-proportion), y = proportion)) + geom_bar(stat = "identity", fill = "skyblue") + geom_hline(yintercept = 30, linetype="dashed", color="red", size =1) + 
  ggtitle("Percentage of 0 values per column") + 
  labs(x="Variable name", y = "Percentage (%)") +
  theme(axis.text.x=element_text(angle=45, hjust=1), 
        plot.title    = element_text(family = "mono", hjust =0.5),
        axis.title.x = element_text(family="mono"),
        axis.title.y = element_text(family="mono")
  ) 

zeros_plot

### dropping all variables, where more than 30% of entries are 0 ###

user_data_small <- user_data_small[ ,(colSums(user_data_small == 0)/nrow(user_data_small) *100) < 30] %>% 
  select(-c( "name", "yelping_since"))

#coding all "none" values as 0, and adding a friend count
user_data_small$friends <- ifelse(user_data_small$friends=="None",0,str_count(user_data_small$friends, ",") + 1)  


# Cleaning business data --------------------------------------------------

colSums(is.na(business_data))

# attempting to predict out missing values for "accepts credit cards" using
# logit model

# training data will be non missing values
# when missing values are removed the majority of values are true
# and the proportion does not align with common sense
# price range does align with common sense however, so non
# missing values will be kept

#accepts_cards_nm <- 
#  business_data[!is.na(business_data$attributes$BusinessAcceptsCreditCards),]

business_data <- unnest(business_data)


# city has been removed due to very uncommon cities may not be common to train and test data
# "state" hopes to capture some of this predictability

business_data_clean <- 
  business_data[!is.na(business_data$RestaurantsPriceRange2),] %>%
  select(c( "stars", "review_count", "is_open", 
             "business_id", "RestaurantsPriceRange2", "state"))



# test data will be None or missing values




# Cleaning review data ----------------------------------------------------






# Merging user, review and business data sets -----------------------------






