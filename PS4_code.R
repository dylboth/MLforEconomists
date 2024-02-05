install.packages('neuralnet')
install.packages('randomForest')
install.packages('doParallel')

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
library(neuralnet)
library(randomForest)
library(doParallel)

setwd('C:/Users/dylan/OneDrive/School/GT/CourseMaterials/fall2023/ECON 4803/Problem_Set_4')
getwd()


#### Analysis I ####

# code from 2.1
#Draw observable explanatory variables
n <- 1000
x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
x5 = rbeta(n,2,1);
x = cbind(x1,x2,x3,x4,x5)
###############################################
#transform into independent random variables
# find the current correlation matrix
c1 <- var(x)
# cholesky decomposition to get independence
chol1 <- solve(chol(c1))
x <- x %*% chol1
###############################################
#generate random correlation matrix
R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
RtR <- R %*% t(R)
corr <- cov2cor(RtR)
# check that it is positive definite
sum((eigen(corr)$values>0))==ncol(x)
################################################
#transform according to this correlation matrix
x <- x %*% chol(corr)
datam <- as.data.frame(x)
datam <- datam %>% dplyr::rename(x1 = V1, x2 = V2, x3 = V3, x4 = V4, x5 = V5)

# calculate y value for specification 1
datam$ y1 <- datam$x1+(datam$x2 * (datam$x3)^2)/10 + (datam$x4 * datam$x1 * datam$x5)/10
# calculate y value for specification 2
u <- rnorm(nrow(datam), mean = 0, sd = 1)
datam$y2 <- log(abs(datam$x1^4 / 10) + abs(datam$x2) + datam$x3^2) + datam$x4 * datam$x2 * sin(datam$x5) + u

# set seed 
set.seed(0)
# number of simulations to run (100 is just an example!)
nsim <- 50

# set parallelization
# detect the number of Cores available in the system
nCores <- parallel::detectCores()
cl <- parallel::makeCluster(nCores);
doParallel::registerDoParallel(cl)



results <- foreach(i=1:nsim, .combine=rbind, .packages = c('neuralnet','randomForest')) %dopar% {
     
    # randomly select half of the data for training
    df <- datam[sample(1:nrow(datam)),]
    row.names(df) <- 1:nrow(df)
    n <- as.integer(0.5 * (nrow(df)))
  
    train_df <- df[row.names(df) %in% 1:n, ]
    test_df <- df[row.names(df) %in% (n+1):nrow(df), ]
    
    #### first specification ####
    
    # fit neural network
    nn <- neuralnet(y1 ~ x1 + x2 + x3 + x4 + x5, train_df,
                    hidden = c(64, 32, 16), threshold = 0.1, linear.output = FALSE)
    # make predictions with neural network
    nn_preds1 <- predict(nn, test_df)
    nn_mse1 <- sum((nn_preds1 - test_df$y1)^2) / n
    
    # fit polynomial regression
    scaled_data <- scale(train_df[, 1:5])
    # Apply a polynomial transformation to the scaled data
    polyn <- poly(scaled_data, degree = 3, raw = TRUE)
    p <- lm(y1 ~ polyn, train_df)
    # make predictions with polynomial
    p_preds1 <- predict(p, test_df)
    p_mse1 <- sum((p_preds1 - test_df$y1)^2) / n
    
    # fit random forest
    # sample 4 covariates
    x_indices <- sample(1:5, 4)
    x_vars <- train_df[, x_indices]
    
    # Fitting the random forest model
    rf <- randomForest(x_vars, test_df$y1, ntree = 1000)
    
    # make predictions from random forest model
    rf_preds1 <- predict(rf, test_df)
    rf_mse1 <- sum((rf_preds1 - test_df$y1)^2) / n
    
    #### second specification ####
    # fit neural network
    nn2 <- neuralnet(y2 ~ x1 + x2 + x3 + x4 + x5, train_df,
                    hidden = c(64, 32, 16), threshold = 0.1)
    # make predictions with neural network
    nn_preds2 <- predict(nn2, test_df)
    nn_mse2 <- sum((nn_preds2 - test_df$y2)^2) / n
    
    # fit polynomial regression
    # Apply a polynomial transformation to the scaled data
    p2 <- lm(y2 ~ polyn, train_df)
    # make predictions with polynomial
    p_preds2 <- predict(p2, test_df)
    p_mse2 <- sum((p_preds2 - test_df$y2)^2) / n
    
    # fit random forest
    rf2 <- randomForest(x_vars, test_df$y2, ntree = 1000)
    
    # make predictions from random forest model
    rf_preds2 <- predict(rf2, test_df)
    rf_mse2 <- sum((rf_preds2 - test_df$y2)^2) / n
    
    example <- c(nn_mse1, p_mse1, rf_mse1, nn_mse2, p_mse2, rf_mse2)
    }

# clean Up
parallel::stopCluster(cl)
rm(cl)  
# average mse values for each model and specification
spec1_mse_list <- colMeans(results[, 1:3])
spec1_mse_list

spec2_mse_list <- colMeans(results[, 4:6])
spec2_mse_list

#### Analysis 2 ####

## Data Cleaning From PS3 solutions ##

# 1. Load the data
airbnb_data <- read.csv("airbnb_data.csv")
airbnb_data$X <- NULL

datam <- airbnb_data[complete.cases(airbnb_data[, c("price", "accommodates", "beds", "number_of_reviews", "review_scores_rating")]),]
datam$host_experience <- difftime(as.Date("2023-06-01"), as.Date(datam$host_since), units = "days")
datam$host_experience <- as.double(datam$host_experience)/365
datam <- datam[complete.cases(datam[, c("host_experience")]),]

datam$entire_apt <- ifelse(datam$room_type == "Entire home/apt", 1, 0)

datam$host_is_superhost[is.na(datam$host_is_superhost)] <- ifelse(((datam$host_response_rate[is.na(datam$host_is_superhost)] >=90) & (datam$number_of_reviews[is.na(datam$host_is_superhost)] >=10) & 
                                                                     (datam$review_scores_rating[is.na(datam$host_is_superhost)] >= 4.8)), 1, 0)

datam <- datam[complete.cases(datam[, c("host_is_superhost")]),]
datam <- datam[order(datam$id),]

# Preliminary data cleaning:
datam$host_identity_verified <- ifelse(datam$host_identity_verified == "t", 1, 0)
datam <- datam[complete.cases(datam[, c("review_scores_rating", "review_scores_accuracy", "review_scores_value")]),]

## end data cleaning ##

## model implementation ## 

## Random Forest Model
rf <- randomForest(host_is_superhost ~ review_scores_rating 
                   + host_identity_verified + review_scores_accuracy + beds 
                   + review_scores_value,
                   data = datam,
                   ntree = 1000)

# make predictions from random forest model
rf_preds <- predict(rf, datam)
thresh <- 0.5
rf_preds <- case_when(rf_preds >= thresh ~ 1,
                     rf_preds < thresh ~ 0)
rf.error <- count(rf_preds != datam$host_is_superhost) / nrow(datam)

## k-means
x_vals <- c('review_scores_rating', 'host_experience', 'beds', 'price')
df <- select(datam, all_of(x_vals))
km_model <- kmeans(df, centers=4)

km_model$cluster

# add column to dataframe assigning each observation to its cluster
datam <- datam %>%
  mutate(cluster = km_model$cluster)

# write a function to get the mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# aggregate data by cluster
sum_df <- group_by(datam, by=datam$cluster) %>% dplyr::summarize(
  m_price = round(mean(price), 2),
  sd_price = round(sd(price),2),
  neighb = get_mode(neighbourhood_group_cleansed),
  prop_type = get_mode(property_type),
  m_review = mean(review_scores_value),
  sd_review = sd(review_scores_value),
  m_list_cnt = mean(host_listings_count),
  sd_list_cnt = sd(host_listings_count),
  m_num_rev = mean(number_of_reviews),
  sd_num_rev = sd(number_of_reviews),
  super = get_mode(host_is_superhost),
  m_beds = mean(beds),
  sd_beds = sd(beds),
  m_exp = mean(host_experience),
  sd_exp = sd(host_experience),
  m_rating = mean(review_scores_rating),
  sd_rating = sd(review_scores_rating)
)
sum_df

# split sum_df into 4 tables for copying into word
# Get total number of columns
num_columns <- ncol(sum_df)

# Calculate the number of columns for each subset
cols_per_subset <- num_columns %/% 4

# Create four dataframes, each containing a quarter of the columns
subset_1 <- sum_df[, 1:cols_per_subset]
subset_2 <- sum_df[, (cols_per_subset + 1):(2 * cols_per_subset)]
subset_3 <- sum_df[, (2 * cols_per_subset + 1):(3 * cols_per_subset)]
subset_4 <- sum_df[, (3 * cols_per_subset + 1):num_columns]

## agglomerative clustering ##
# Calculate the distance matrix
distance_matrix <- dist(df)  
hierarchical_cluster <- hclust(distance_matrix, method = "complete")  

# Cut the dendrogram into k=4 clusters
clusters <- cutree(hierarchical_cluster, k = 4)
clusters

# add column to dataframe assigning each observation to its cluster
datam2 <- datam %>%
  mutate(cluster = clusters)

# aggregate data by cluster
sum_df2 <- group_by(datam2, by=datam2$cluster) %>% dplyr::summarize(
  m_price = round(mean(price), 2),
  sd_price = round(sd(price),2),
  neighb = get_mode(neighbourhood_group_cleansed),
  prop_type = get_mode(property_type),
  m_review = mean(review_scores_value),
  sd_review = sd(review_scores_value),
  m_list_cnt = mean(host_listings_count),
  sd_list_cnt = sd(host_listings_count),
  m_num_rev = mean(number_of_reviews),
  sd_num_rev = sd(number_of_reviews),
  super = get_mode(host_is_superhost),
  m_beds = mean(beds),
  sd_beds = sd(beds),
  m_exp = mean(host_experience),
  sd_exp = sd(host_experience),
  m_rating = mean(review_scores_rating),
  sd_rating = sd(review_scores_rating)
)
sum_df2

# split sum_df2 into 4 tables for copying into word

# Create four dataframes, each containing a quarter of the columns
sub_1 <- sum_df2[, 1:cols_per_subset]
sub_2 <- sum_df2[, (cols_per_subset + 1):(2 * cols_per_subset)]
sub_3 <- sum_df2[, (2 * cols_per_subset + 1):(3 * cols_per_subset)]
sub_4 <- sum_df2[, (3 * cols_per_subset + 1):num_columns]




