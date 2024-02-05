# declare libraries
library(abd)
library(Hmisc)
library(lattice)
library(mosaic)
library(dplyr)
library(psych) 
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyr)
library(leaps)
library(glmnet)
library(stringi)



setwd('C:/Users/dylan/OneDrive/School/GT/CourseMaterials/fall2023/ECON 4803/Problem_Set_2')
getwd()

#### Empirical Problems ####

### Data Cleaning (from PS2) ###

# (a) bring data into R
df <- read.csv('airbnb_data.csv')
# (b) remove NA values from the price column
df <- df[!is.na(df$price), ]
# (c) remove NA values from accommodates, beds, number_of_reviews, and review_scores_rating columns
cols <- c('accommodates', 'beds', 'number_of_reviews', 'review_scores_rating')
df <- df %>% drop_na(cols)
# (d) create host_experience variable
df$host_experience <- (as.Date("6/5/2023", format="%m/%d/%Y") - as.Date(df$host_since, format="%m/%d/%Y")) / 365
# get rid of NA values
df <- df[!is.na(df$host_experience), ]
# (e) create entire_apt variable
df$entire_apt <- case_when(df$room_type == "Entire home/apt" ~ 1, df$room_type != "Entire home/apt" ~ 0)
sum(is.na(df$entire_apt)) # this equals 0
# there are no NA values to remove

# (f) appropriately fill NA values in host_is_superhost
df$host_is_superhost <- case_when(df$host_is_superhost == 1 ~ 1,
                                  df$host_is_superhost == 0 ~ 0,
                                  df$host_response_rate >= 90 & df$number_of_reviews >= 10 & df$review_scores_rating >= 4.8 ~ 1,
                                  .default = NA
)
# (g) remove NA values from host_is_superhost
df <- df[!is.na(df$host_is_superhost), ]
# (h) sort by id
df <- df[order(df$id), ]

#### 1 ####
### (a) ###

df$host_identity_verified <- ifelse(df$host_identity_verified == 't', 1, df$host_identity_verified)
df$host_has_profile_pic <- ifelse(df$host_has_profile_pic == 't', 1, df$host_identity_verified)
df$instant_bookable <- ifelse(df$instant_bookable == 't', 1, df$host_identity_verified)

### (b) ###
df <- df[!is.na(df$review_scores_rating), ]
df <- df[!is.na(df$review_scores_accuracy), ]
df <- df[!is.na(df$review_scores_value), ]

#### 2 ####
# (a) set seed to 0
set.seed(0)
# randomly select ten-percent of the data for training
# randomly shuffle df
df <- df[sample(1:nrow(df)), ]
row.names(df) <- 1:nrow(df)
n <- as.integer(0.9 * (nrow(df)))

train_df <- df[row.names(df) %in% 1:n, ]
test_df <- df[row.names(df) %in% (n+1):nrow(df), ]



# (b) linear probability model
lin.model <- lm(host_is_superhost ~ review_scores_rating, data=train_df)
# (c) logistic regression model
log.model <- glm(host_is_superhost ~ review_scores_rating, data=train_df,family=binomial)
# (d) probit model
prob.model <- glm(host_is_superhost ~ review_scores_rating, data=train_df,family=binomial(link = "probit"))

# (e) get predictions
lin.pred <- unique(predict(lin.model, newdata = test_df, type="response"))
log.pred <- unique(predict(log.model, newdata = test_df, type="response"))
prob.pred <- unique(predict(prob.model, newdata = test_df, type="response"))

# plot the fitted values
test_df <- test_df %>% arrange(review_scores_rating)
x_axis <- test_df %>%group_by(review_scores_rating) %>%
  summarise(review_scores_rating = first(review_scores_rating))


# plot evolution of average hub height and rotor diameter at the project level
p <-  
  ggplot(data=x_axis, aes(x=review_scores_rating)) + 
  geom_line(aes(y=lin.pred, colour='Linear Probability Model')) + 
  geom_line(aes(y=log.pred, colour='Logit Model')) +
  geom_line(aes(y=prob.pred, colour='Probit Model')) +
  ylab('Probability') +
  ggtitle('Probability of Being a SuperHost') +
  scale_colour_manual("", 
                      breaks = c('Linear Probability Model', 'Logit Model', 'Probit Model'),
                      values = c('Linear Probability' = 'red', 'Logit Model' = 'blue', 'Probit Model' ='green'))



plotly::ggplotly(p)

coef(lin.model)
coef(log.model)
coef(prob.model)

# (f) fit l1 logistic regression
X <- model.matrix(host_is_superhost ~ review_scores_rating +
                    host_experience + 
                    host_identity_verified + 
                    review_scores_accuracy +
                    beds + 
                    review_scores_value +
                    I(as.numeric(review_scores_rating)^2) +
                    I(as.numeric(host_experience)^2) +
                    I(as.numeric(review_scores_accuracy)^2) +
                    I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(host_experience)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(review_scores_accuracy)^2) + 
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(host_experience)^2) * I(as.numeric(review_scores_accuracy)^2) +
                    I(as.numeric(host_experience)^2) * I(as.numeric(beds)^2) + 
                    I(as.numeric(host_experience)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(review_scores_accuracy)^2) * I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_accuracy)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(beds)^2) * I(as.numeric(review_scores_value)^2), data=train_df)[,-1]

y <- train_df$host_is_superhost
# implement cross validation to find the optimal penalty term for l1 regularized logit regression
l1.model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
best_lambda <- l1.model$lambda.min
coef(l1.model, s = best_lambda)

test_X <- model.matrix(host_is_superhost ~ review_scores_rating +
                    host_experience + 
                    host_identity_verified + 
                    review_scores_accuracy +
                    beds + 
                    review_scores_value +
                    I(as.numeric(review_scores_rating)^2) +
                    I(as.numeric(host_experience)^2) +
                    I(as.numeric(review_scores_accuracy)^2) +
                    I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(host_experience)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(review_scores_accuracy)^2) + 
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_rating)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(host_experience)^2) * I(as.numeric(review_scores_accuracy)^2) +
                    I(as.numeric(host_experience)^2) * I(as.numeric(beds)^2) + 
                    I(as.numeric(host_experience)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(review_scores_accuracy)^2) * I(as.numeric(beds)^2) +
                    I(as.numeric(review_scores_accuracy)^2) * I(as.numeric(review_scores_value)^2) +
                    I(as.numeric(beds)^2) * I(as.numeric(review_scores_value)^2), data=test_df)[,-1]

# calculate the percentage of hosts that are superhosts in the training data
count(train_df[train_df$host_is_superhost == 1, ]) / nrow(train_df)
# define the threshold for predicting a superhost
thresh <- 0.38

y_test <- test_df$host_is_superhost
nrow(train_df[train_df$review_scores_rating > 4.94, ]) / nrow(train_df)
max(lin.pred)

# get the mean squared prediction error for the linear probability model
lin.pred <- predict(lin.model, newdata = test_df, type="response")
lin.pred <- case_when(lin.pred >= thresh ~ 1,
                     lin.pred < thresh ~ 0)
lin.sse <- count(lin.pred != y_test) / nrow(test_df)
# get the mean squared prediction error for the logit model
log.pred <- predict(log.model, newdata = test_df, type="response")
log.pred <- case_when(log.pred >= thresh ~ 1,
                     log.pred < thresh ~ 0)
log.sse <- count(log.pred != y_test) / nrow(test_df)
# get the mean squared prediction error for the probit model
prob.pred <- predict(prob.model, newdata = test_df, type="response")
prob.pred <- case_when(prob.pred >= thresh ~ 1,
                     prob.pred < thresh ~ 0)
prob.sse <- count(prob.pred != y_test) / nrow(test_df)

# get the mean prediction error for the regularized model
l1.pred <- predict(l1.model, s=best_lambda, newx=test_X, type='response')
l1.pred <- case_when(l1.pred >= thresh ~ 1,
                     l1.pred < thresh ~ 0)
l1.sse <- count(l1.pred != y_test) / nrow(test_df)




