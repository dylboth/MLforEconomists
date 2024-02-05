install.packages("leaps")
install.packages("glmnet")
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

### (1) Data Cleaning ###

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

### (2) and (3) Analysis ###

# add noise variables to original data

df$noise1 <- df$host_experience + rnorm(nrow(df), .01)
df$noise2 <- df$host_is_superhost + rnorm(nrow(df), .01)
df$noise3 <- df$number_of_reviews + rnorm(nrow(df), .01)


# create a function that runs each regression for a given training sample size and boolean noise variable
model_data <- function(ts, noise) {
  mse_list <- c()
  rsq_list <- c()
  # (a) set seed to 0
  set.seed(0)
  # (b) randomly select half of the data for training
  # randomly shuffle df
  df <- df[sample(1:nrow(df)), ]
  row.names(df) <- 1:nrow(df)
  # select the first half of the shuffled data for training and the second half for testing
  n <- as.integer(ts * (nrow(df)))
 
  train_df <- df[row.names(df) %in% 1:n, ]
  test_df <- df[row.names(df) %in% (n+1):nrow(df), ]
  
  if (noise == TRUE) {
    train_x <- model.matrix(price ~ accommodates + beds + 
                              host_experience + number_of_reviews + 
                              review_scores_rating +
                              entire_apt + host_is_superhost + noise1 + noise2 + noise3 + I(accommodates^2) + I(beds^2) + 
                              I(as.numeric(host_experience)^2) + I(review_scores_rating^2) + I(number_of_reviews^2) +
                              (accommodates * beds) + (accommodates * host_experience) +
                              (accommodates * number_of_reviews) + (accommodates * review_scores_rating) + 
                              (beds * host_experience) + (beds * number_of_reviews) + (beds * review_scores_rating) +
                              (host_experience * number_of_reviews) + (host_experience * review_scores_rating) + 
                              (number_of_reviews * review_scores_rating), train_df)[,-1]
    test_x <- model.matrix(price ~ accommodates + beds + 
                              host_experience + number_of_reviews + 
                              review_scores_rating +
                              entire_apt + host_is_superhost + noise1 + noise2 + noise3 + I(accommodates^2) + I(beds^2) + 
                              I(as.numeric(host_experience)^2) + I(review_scores_rating^2) + I(number_of_reviews^2) +
                              (accommodates * beds) + (accommodates * host_experience) +
                              (accommodates * number_of_reviews) + (accommodates * review_scores_rating) + 
                              (beds * host_experience) + (beds * number_of_reviews) + (beds * review_scores_rating) +
                              (host_experience * number_of_reviews) + (host_experience * review_scores_rating) + 
                              (number_of_reviews * review_scores_rating), test_df)[,-1]
    nvars <- 10
  } else {
    train_x <- model.matrix(price ~ accommodates + beds + 
                              host_experience + number_of_reviews + 
                              review_scores_rating +
                              entire_apt + host_is_superhost + I(accommodates^2) + I(beds^2) + 
                              I(as.numeric(host_experience)^2) + I(review_scores_rating^2) + I(number_of_reviews^2) +
                              (accommodates * beds) + (accommodates * host_experience) +
                              (accommodates * number_of_reviews) + (accommodates * review_scores_rating) + 
                              (beds * host_experience) + (beds * number_of_reviews) + (beds * review_scores_rating) +
                              (host_experience * number_of_reviews) + (host_experience * review_scores_rating) + 
                              (number_of_reviews * review_scores_rating), train_df)[,-1]
    test_x <- model.matrix(price ~ accommodates + beds + 
                              host_experience + number_of_reviews + 
                              review_scores_rating +
                              entire_apt + host_is_superhost + I(accommodates^2) + I(beds^2) + 
                              I(as.numeric(host_experience)^2) + I(review_scores_rating^2) + I(number_of_reviews^2) +
                              (accommodates * beds) + (accommodates * host_experience) +
                              (accommodates * number_of_reviews) + (accommodates * review_scores_rating) + 
                              (beds * host_experience) + (beds * number_of_reviews) + (beds * review_scores_rating) +
                              (host_experience * number_of_reviews) + (host_experience * review_scores_rating) + 
                              (number_of_reviews * review_scores_rating), test_df)[,-1]
    nvars <- 7
  }
  
  train_y <- train_df$price
  
  test_y <- test_df$price

  
  # (c) initial linear regression
  model <- lm(price ~ train_x[, 1:nvars], train_df)

  
  # compute the R-Squared of this model
  y_bar <- mean(train_df$price)
  TSS <- sum((train_df$price - y_bar)^2)
  RSS <- sum((train_df$price - predict(model, train_df))^2)
  R_Squared <- 1 - (RSS / TSS)
  rsq_list <- append(rsq_list, R_Squared)
  # compute the MSE
  MSE <- mean((test_df$price - predict(model, test_df))^2)
  mse_list <- append(mse_list, MSE)
  
  # (d) model with second order terms
  second_model <- lm(price ~ train_x, train_df)
  
  # compute the R-Squared of this model
  RSS_2 <- sum((train_df$price - predict(second_model, train_df))^2)
  R_Squared_2 <- 1 - (RSS_2 / TSS)
  rsq_list <- append(rsq_list, R_Squared_2)
  # compute the MSE
  MSE_2 <- mean((test_y - predict(second_model, test_df))^2)
  mse_list <- append(mse_list, MSE_2)

  # (e) perform simple backwards step-wise selection on second_model
  regfit.bwd <- regsubsets(price ~ train_x, train_df, nvmax=nvars+16, method='backward')
  sum <- summary(regfit.bwd)
  # get the best model based on R^2
  best_rsq <- which.max(sum$rsq)
  # get the coefficient from the best R^2 model
  names <- names(coef(regfit.bwd, best_rsq))[2:length(coef(regfit.bwd, best_rsq))]
  vars <- substr(names, 8, nchar(names))
  bwd_rsq_model <- matrix(coef(regfit.bwd, best_rsq))
  # add the R-Squared of this model
  rsq_list <- append(rsq_list, max(sum$rsq))
  
  # compute the MSE
  y_pred <- cbind(rep(1, nrow(test_df)), test_x[, vars]) %*% bwd_rsq_model
  mse_best_rsq <- mean((test_y - c(y_pred))^2)
  mse_list <- append(mse_list, mse_best_rsq)
  
  # get the best model based on BIC
  best_bic <- which.min(sum$bic)
  # get coefficients in the best BIC model
  names_bic <- names(coef(regfit.bwd, best_bic))[2:length(coef(regfit.bwd, best_bic))]
  vars <- substr(names_bic, 8, nchar(names_bic))
  bwd_bic_model <- matrix(coef(regfit.bwd, best_bic))
  
  # add the R-Squared of this model
  rsq_list <- append(rsq_list, sum$rsq[best_bic])

  
  # compute the MSE
  y_pred <- cbind(rep(1, nrow(test_df)), test_x[, vars]) %*% bwd_bic_model
  mse_best_bic <- mean((test_y - y_pred)^2)
  mse_list <- append(mse_list, mse_best_bic)
  
  # (f) fit ridge and lasso models
  lambdas <- c(0,5,10)
  ridge_model <- glmnet(train_x,train_y,alpha=0,lambda=lambdas, standardize=FALSE)
  lasso_model <- glmnet(train_x,train_y,alpha=1,lambda=lambdas, standardize=FALSE)
  
  # compute test MSE for each model
  ridge_0_mse <- mean((test_df$price - predict(ridge_model, s=0, newx=test_x))^2)
  ridge_5_mse <- mean((test_df$price - predict(ridge_model, s=5, newx=test_x))^2)
  ridge_10_mse <- mean((test_df$price - predict(ridge_model, s=10, newx=test_x))^2)
  mse_list <- append(mse_list, ridge_0_mse)
  mse_list <- append(mse_list, ridge_5_mse)
  mse_list <- append(mse_list, ridge_10_mse)
  # compute the R-Squared values for ridge models
  y_bar <- mean(test_df$price)
  TSS <- sum((test_df$price - y_bar)^2)
  ridge_0_rss <- sum((test_df$price - predict(ridge_model, s=0, newx=test_x))^2)
  ridge_5_rss <- sum((test_df$price - predict(ridge_model, s=5, newx=test_x))^2)
  ridge_10_rss <- sum((test_df$price - predict(ridge_model, s=10, newx=test_x))^2)
  R_Squared_ridge0 <- 1 - (ridge_0_rss / TSS)
  R_Squared_ridge5 <- 1 - (ridge_5_rss / TSS)
  R_Squared_ridge10 <- 1 - (ridge_10_rss / TSS)
  rsq_list <- append(rsq_list, R_Squared_ridge0)
  rsq_list <- append(rsq_list, R_Squared_ridge5)
  rsq_list <- append(rsq_list, R_Squared_ridge10)
  
  lasso_0_mse <- mean((test_df$price - predict(lasso_model, s=0, newx=test_x))^2)
  lasso_5_mse <- mean((test_df$price - predict(lasso_model, s=5, newx=test_x))^2)
  lasso_10_mse <- mean((test_df$price - predict(lasso_model, s=10, newx=test_x))^2)
  mse_list <- append(mse_list, lasso_0_mse)
  mse_list <- append(mse_list, lasso_5_mse)
  mse_list <- append(mse_list, lasso_10_mse)
  # compute the R-Squared values for lasso models
  lasso_0_rss <- sum((test_df$price - predict(lasso_model, s=0, newx=test_x))^2)
  lasso_5_rss <- sum((test_df$price - predict(lasso_model, s=5, newx=test_x))^2)
  lasso_10_rss <- sum((test_df$price - predict(lasso_model, s=10, newx=test_x))^2)
  R_Squared_lasso0 <- 1 - (lasso_0_rss / TSS)
  R_Squared_lasso5 <- 1 - (lasso_5_rss / TSS)
  R_Squared_lasso10 <- 1 - (lasso_10_rss / TSS)
  rsq_list <- append(rsq_list, R_Squared_lasso0)
  rsq_list <- append(rsq_list, R_Squared_lasso5)
  rsq_list <- append(rsq_list, R_Squared_lasso10)
  
  # get coefficients for each lambda
  ridge_0 <- coef(ridge_model)[, 3]
  ridge_5 <-  coef(ridge_model)[, 2]
  ridge_10 <-  coef(ridge_model)[, 1]
  
  lasso_0 <- coef(lasso_model)[, 3]
  lasso_5 <- coef(lasso_model)[, 2]
  lasso_10 <- coef(lasso_model)[, 1]
  
  
  # create summary table
  sum_table <- data.frame(ridge_0, ridge_5, ridge_10, lasso_0, lasso_5, lasso_10)
  colnames(sum_table) <- c('Ridge (lambda=0)', 'Ridge (lambda=5)', 'Ridge (lambda=10)', 'Lasso (lambda=0)', 'Lasso (lambda=5)', 'Lasso (lambda=10)')
  # print(sum_table)
  
  # (g) use 10-fold cross-validation to find optimal tuning parameter for both ridge and lasso using training data
  
  # cross validation for ridge
  cv_ridge <- cv.glmnet(train_x, train_y, alpha=0)
  best_ridge <- cv_ridge$lambda.min
  
  # mse for optimal ridge model
  mse_ridge <- mean((test_y - predict(ridge_model, s=best_ridge, newx=test_x))^2)
  mse_list <- append(mse_list, mse_ridge)
  # compute the R-Squared of this model
  RSS_cvr <- sum((test_df$price - predict(ridge_model, s=best_ridge, newx=test_x))^2)
  R_Squared_cvr <- 1 - (RSS_cvr / TSS)
  rsq_list <- append(rsq_list, R_Squared_cvr)
  
  # cross validation for lasso
  cv_lasso <- cv.glmnet(train_x, train_y, alpha=1)
  best_lasso <- cv_lasso$lambda.min
  
  # mse for optimal ridge model
  mse_lasso <- mean((test_y - predict(lasso_model, s=best_lasso, newx=test_x))^2)
  mse_list <- append(mse_list, mse_lasso)
  # compute the R-Squared of this model
  RSS_cvl <- sum((test_df$price - predict(lasso_model, s=best_ridge, newx=test_x))^2)
  R_Squared_cvl <- 1 - (RSS_cvl / TSS)
  rsq_list <- append(rsq_list, R_Squared_cvl)
  
  return(c(mse_list, rsq_list))
}

# run regressions with 50% training split and no noise
model_50 <- model_data(0.5, FALSE)
model_50_mse <- model_50[1:12]
model_50_rsq <- model_50[13:24]

# run regressions with 50% training split and noise
model_50_noise <- model_data(0.5, TRUE)
model_50_noise_mse <- model_50_noise[1:12]
model_50_noise_rsq <- model_50_noise[13:24]
# run regressions with 10% training split and no noise
model_10 <- model_data(0.1, FALSE)
model_10_mse <- model_10[1:12]
model_10_rsq <- model_10[13:24]
# run regressions with 10% training split and noise
model_10_noise <- model_data(0.1, TRUE)
model_10_noise_mse <- model_10_noise[1:12]
model_10_noise_rsq <- model_10_noise[13:24]
# run regressions with 2% training split and no noise
model_2_per <- model_data(0.02, FALSE)
model_2_per_mse <- model_2_per[1:12]
model_2_per_rsq <- model_2_per[13:24]
# run regressions with 2% training split and noise
model_2_noise <- model_data(0.02, TRUE)
model_2_noise_mse <- model_2_noise[1:12]
model_2_noise_rsq <- model_2_noise[13:24]
