library(dplyr)
fld_p_data<-read.csv('playground-series-s4e5/train.csv')
head(fld_p_data)
summary(fld_p_data)
fld <-fld_p_data%>%
  select(-id)
  
head(fld)

set.seed(42)
numOfTrain <- round(nrow(fld) * 0.7)
train_fld <- fld[1:numOfTrain,]
test_fld <- fld_x[(numOfTrain + 1):length(data_y), ]

mdl<-glm(FloodProbability ~ ., data = fld, family = gaussian(link = "identity"))
summary(mdl)
mdl.
# library(car)
# vif(glm(FloodProbability ~ ., data = cbind(train_fldx, train_fldy), family = binomial(link = "logit")))
# 
# cor_matrix <- cor(train_fldx)
# melted_cor_matrix <- melt(cor_matrix)
# 
# # Plot heatmap
# ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1, 1), space = "Lab", 
#                        name="Correlation") +
#   theme_minimal() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 12, hjust = 1)) +
#   coord_fixed()
# highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9) # cutoff can be adjusted
# train_fldx_reduced <- train_fldx[, -highly_correlated]
# 
# head(train_fldx)

























library(xgboost) # for xgboost
library(tidyverse)
library(ggplot2)

library(reshape2)

# Load the data
data <- read.csv("kerala.csv")
head(data)
cor_matrix <- cor(data %>% select_if(is.numeric) %>%
                    select(-'YEAR'))

# Melt the correlation matrix
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()
# Select numeric columns and exclude the 'YEAR' column
data_x <- data %>%
  select_if(is.numeric) %>%
  select(-'YEAR')
head(data_x)

# Convert the selected data to a numeric matrix
mx <- data.matrix(data_x)

# Convert the 'FLOODS' column to a binary numeric vector
data_y <- ifelse(data$FLOODS == 'YES', 1, 0)
head(data_y)
data_combined <- cbind(data_x, FLOODS = data_y)

set.seed(123)
numOfTrain <- round(length(data_y) * 0.5)
train_x <- mx[1:numOfTrain, ]
train_y <- data_y[1:numOfTrain]
test_x <- mx[(numOfTrain + 1):length(data_y), ]
test_y <- data_y[(numOfTrain + 1):length(data_y)]

# fit<-glm(train_y~train_x,family =binomial(link = "logit") )






# Create DMatrix for xgboost
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Train the model
model <- xgboost(data = dtrain, nround = 5, objective = "binary:logistic")

# Make predictions
pred <- predict(model, dtest)

# Calculate and print the classification error
err <- mean(as.numeric(pred > 0.5) != test_y)
print(paste("test-error=", err))





#
library(MASS)
N <- 5  
set.seed(42)

mu <- c(10,0,-5,6,-20)
n=10000

Sigma <- matrix(runif(N * N, -1, 1), nrow = N)
Sigma <- Sigma %*% t(Sigma)  

mv_sample <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
plot(density(mv_sample))
plos<-function(params,data,N){
  q<-1
  mu<-params[1:N]
  e<-eigen(cor(data))
  p<-e$vectors
  pp<-eigen(Sigma)$vectors
  dis<- 0
  for(i in 1:nrow(data)){
    dis<-dis+sum((t(p)%*%(data[i,]-mu))^2)
  }
  dis/nrow(data)#+q*sum(mu^2)
}

sqe<-function(params,data,N){
  q<-0
  mu<-params[1:N]
  sum((data-t(matrix(mu,N,nrow(data))))^2)/nrow(data)+q*sum(mu^2)
}

intp<-c(1,0,0,0,0)
optim(par=intp,fn=plos,data=mv_sample,N=N,method="L-BFGS-B")$par
optim(par=intp,fn=sqe,data=mv_sample,N=N,method="L-BFGS-B")$par
apply(mv_sample,2, mean)



library(xgboost) # for xgboost
library(tidyverse)
library(ggplot2)



set.seed(42) 
# Load the data
data <- read.csv("../kerala.csv")
head(data)
data<-data[sample(nrow(data)), ]
head(data)
# Select numeric columns and exclude the 'YEAR' column
data_x <- data %>%
  select_if(is.numeric) %>%
  select(- YEAR)
head(data_x)

# Convert the selected data to a numeric matrix
mx <- data.matrix(data_x)

# Convert the 'FLOODS' column to a binary numeric vector
data_y <- ifelse(data$FLOODS == 'YES', 1, 0)
head(data_y)
data_combined <- cbind(data_x, FLOODS = data_y)

numOfTrain <- round(length(data_y) * 0.6)
train_x <- mx[1:numOfTrain, ]
train_y <- data_y[1:numOfTrain]
test_x <- mx[(numOfTrain + 1):length(data_y), ]
test_y <- data_y[(numOfTrain + 1):length(data_y)]

# Create DMatrix for xgboost
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Train the model
model <- xgboost(data = dtrain, nround = 2, objective = "binary:logistic")

# Make predictions
pred_r <- predict(model, dtrain)
pred_e <- predict(model, dtest)

# Calculate and print the classification error
err_r<- mean(as.numeric(pred_r > 0.5) != train_y)
err_e<- mean(as.numeric(pred_e > 0.5) != test_y)
print(paste("trian-error=", err_r))
print(paste("test-error=", err_e))
# # Extract model parameters
# model_dump <- xgb.dump(model, with_stats = TRUE)
# 
# cat(model_dump[1:10], sep = "\n")
# 
# xgb.dump(model, with_stats = TRUE, fname = "xgboost_model_dump.txt")


custom_objective <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  q <- 0.1
  a_minus_Y <- preds - labels
  indicator <- ifelse(a_minus_Y > 0, 1, 0)
  grad <- (indicator - q)
  
  hess <- rep(0.1, length(grad))
  
  return(list(grad = grad, hess = hess))
}


model <- xgboost(data = dtrain,
                 nround = 2,
                 objective = custom_objective)

err_r<- mean(as.numeric(pred_r > 0.5) != train_y)
err_e<- mean(as.numeric(pred_e > 0.5) != test_y)
print(paste("trian-error=", err_r))
print(paste("test-error=", err_e))

linex <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  
  psi <- 10
  z <- psi * (preds - labels)
  
  grad <- psi*(exp(z) - 1)
  hess <- (psi^2)*exp(z)
  
  return(list(grad = grad, hess = hess))
}

# Train the model with the custom objective function
model <- xgboost(data = dtrain,
                 nround = 2,
                 objective = linex,
                 eval_metric = "error")

pred_r <- predict(model, dtrain)
pred_e <- predict(model, dtest)

err_r<- mean(as.numeric(pred_r > 0.5) != train_y)
err_e<- mean(as.numeric(pred_e > 0.5) != test_y)
print(paste("trian-error=", err_r))
print(paste("test-error=", err_e))

