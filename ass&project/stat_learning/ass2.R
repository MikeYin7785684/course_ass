
# standardize on each country data
he <- read.csv("HE.csv")
country_name <- he[,1]
he <- t(scale(t(he[,-1])))
gdp <- read.csv("GDP.csv")
gdp <- t(scale(t(gdp[,-1])))
dr_young <- read.csv("DR_young.csv")
dr_young <- t(scale(t(dr_young[,-1])))
dr_old <- read.csv("DR_old.csv")
dr_old <- t(scale(t(dr_old[,-1])))
ghe <- read.csv("GHE.csv")
ghe <- t(scale(t(ghe[,-1])))

train_part <- 1:(2000-1971+1)
test_part <- -train_part

he_train <- he[,train_part]
he_test <- he[,test_part]

gdp_train <- gdp[,train_part]
gdp_test <- gdp[,test_part]

dr_young_train <- dr_young[,train_part]
dr_young_test <- dr_young[,test_part]

dr_old_train <- dr_old[,train_part]
dr_old_test <- dr_old[,test_part]

ghe_train <- ghe[,train_part]
ghe_test <- ghe[,test_part]

mse_train <- numeric(nrow(he_train))
mse_test <- numeric(nrow(he_train))
# Q1
loess.gcv <- function(data){
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  x4 <- data$x4
  y  <- data$y
  nobs <- length(y)
  tune.loess <- function(s){
    lo <- loess(y ~ x1 + x2 + x3 +x4, span = s)
    mean((lo$fitted - y)^2) / (1 - lo$trace.hat/nobs)^2
  }
  os <- optimize(tune.loess, interval = c(.01, 99))$minimum
  lo <- loess(y ~ x1 + x2 + x3 + x4, span = os)
  mse <- mean((y-lo$fitted)^2)
  list(model = lo,mse = mse)
}
# kernel 
for (i in 1:nrow(he)) {
  data <- data.frame(
    y = he[i, ],
    x1 = gdp[i, ],
    x2 = dr_young[i, ],
    x3 = dr_old[i, ],
    x4 = ghe[i, ]
  )
  model<-loess.gcv(data)$model
  y_train <- model$fitted[train_part]
  y_test <- model$fitted[test_part]
  mse_train[i] <- mean((y_train-unlist(data$y[train_part]))^2)
  mse_test[i] <- mean((y_test-unlist(data$y[test_part]))^2)
  cat("Country: ",country_name[i], "model:",model,"\n",
      "Training prediction:",y_train , "\n",
      "Testing prediction:",y_test , "\n",
      "Training MSE:",mse_train[i] , "\n",
      "Testing MSE:",mse_test[i] , "\n")
}
mean(mse_train)
mean(mse_test)

# projection spline(gdp)
for (i in 1:nrow(he)) {
  # data <- data.frame(
  #   y = he[i, ],
  #   x1 = gdp[i, ],
  #   x2 = dr_young[i, ],
  #   x3 = dr_old[i, ],
  #   x4 = ghe[i, ]
  # )
  model<-smooth.spline(he[i,train_part],gdp[i,train_part])
  y_train <- unlist(model$y)
  y_test <- unlist(predict(model,gdp[i,test_part])$y)
  mse_train[i] <- mean((y_train-unlist(data$y[train_part]))^2)
  mse_test[i] <- mean((y_test-unlist(data$y[test_part]))^2)
  cat("Country: ",country_name[i], "model:",model,"\n",
      "Training prediction:",y_train , "\n",
      "Testing prediction:",y_test , "\n",
      "Training MSE:",mse_train[i] , "\n",
      "Testing MSE:",mse_test[i] , "\n")
}
mean(mse_train)
mean(mse_test)

# pca spline
for (i in 1:nrow(he)) {
  X_train <- data.frame(
    x1 = gdp_train[i, ],
    x2 = dr_young_train[i, ],
    x3 = dr_old_train[i, ],
    x4 = ghe_train[i, ]
  )
  X_test <- data.frame(
    x1 = gdp_test[i, ],
    x2 = dr_young_test[i, ],
    x3 = dr_old_test[i, ],
    x4 = ghe_test[i, ]
  )
  rot <- prcomp(X_train)$rotation[,1]
  x_train <- rot %*% t(as.matrix(X_train))
  x_test <- rot %*% t(as.matrix(X_test))
  model<-smooth.spline(x_train,he[i,train_part])
  y_train <- unlist(model$y)
  y_test <- unlist(predict(model,x_test)$y)
  mse_train[i] <- mean((y_train-unlist(data$y[train_part]))^2)
  mse_test[i] <- mean((y_test-unlist(data$y[test_part]))^2)
  cat("Country: ",country_name[i], "model:",model,"\n",
      "Training prediction:",y_train , "\n",
      "Testing prediction:",y_test , "\n",
      "Training MSE:",mse_train[i] , "\n",
      "Testing MSE:",mse_test[i] , "\n")
}

mean(mse_train)
mean(mse_test)


# Q2

# pca spline

X_train <- rbind(matrix(gdp_train,1),
  matrix(dr_old_train,1),
  matrix(dr_young_train,1),
  matrix(ghe_train,1)
  )
  
X_test <- rbind(matrix(gdp_test,1),
                 matrix(dr_old_test,1),
                 matrix(dr_young_test,1),
                 matrix(ghe_test,1)
)
# projection spline
x_train <- X_train[1,]
x_test <- X_test[1,]
model<-smooth.spline(x_train,unlist(he[,train_part]))
y_train <- unlist(model$y)
y_test <- unlist(predict(model,x_test)$y)
mse_train <- mean((y_train-unlist(data$y[train_part]))^2)
mse_test <- mean((y_test-unlist(data$y[test_part]))^2)
cat("Training prediction:",mse_train , "\n")
cat("Testing prediction:",mse_test , "\n")

# pca spline
rot <- prcomp(t(X_train))$rotation[,1]
x_train <- rot %*% (as.matrix(X_train))
x_test <- rot %*% (as.matrix(X_test))
model<-smooth.spline(x_train,unlist(he[,train_part]))
y_train <- unlist(model$y)
y_test <- unlist(predict(model,x_test)$y)
mse_train <- mean((y_train-unlist(data$y[train_part]))^2)
mse_test <- mean((y_test-unlist(data$y[test_part]))^2)
cat("Training prediction:",mse_train , "\n")
cat("Testing prediction:",mse_test , "\n")


mse_train <- numeric(nrow(he_train))
mse_test <- numeric(nrow(he_train))
model <- list()


#generalized additive kernel
for (i in 1:nrow(he_train)) {
  data_train <- data.frame(
    y = he_train[i, ],
    x1 = gdp_train[i, ],
    x2 = dr_young_train[i, ],
    x3 = dr_old_train[i, ],
    x4 = ghe_train[i, ]
  )
  data_test <- data.frame(
    y = he_test[i, ],
    x1 = gdp_test[i, ],
    x2 = dr_young_test[i, ],
    x3 = dr_old_test[i, ],
    x4 = ghe_test[i, ]
  )
  s <- seq(0.1,1,.1)
  model.tune <- list()  
  mse.tune <- numeric(length(s))
  for (j in seq_along(s)){
    model.tune[[j]] <- gam(y ~ lo(x1,span = s[j])+lo(x2, span = s[j])+lo(x3, span = s[j])+lo(x4, span = s[j]),data = data_train)
    y_pred_test <- predict(model.tune[[j]], newdata = data_test)
    mse.tune[j] <-mean((y_pred_test-unlist(data_test['y']))^2)
  }
  index <- which.min(mse.tune)
  model[[i]] <- model.tune[[index]]
  y_pred_train <- predict(model.tune[[index]], newdata = data_train)
  mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
  cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
  # y_pred_test <- predict(model, newdata = data_test)
  mse_test[i] <- mse.tune[index]
  cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
}

mse_train <- numeric(nrow(he_train))
mse_test <- numeric(nrow(he_train))
model <- list()

#generalized additive spline
for (i in 1:nrow(he_train)) {
  data_train <- data.frame(
    y = he_train[i, ],
    x1 = gdp_train[i, ],
    x2 = dr_young_train[i, ],
    x3 = dr_old_train[i, ],
    x4 = ghe_train[i, ]
  )
  data_test <- data.frame(
    y = he_test[i, ],
    x1 = gdp_test[i, ],
    x2 = dr_young_test[i, ],
    x3 = dr_old_test[i, ],
    x4 = ghe_test[i, ]
  )
  sp <- seq(0.1,1,.1)
  model.tune <- list()  
  mse.tune <- numeric(length(sp))
  for (j in seq_along(sp)){
    tryCatch({
    model.tune[[j]] <- gam(y ~ s(x1,spar = sp[j])+s(x2, spar = sp[j])+s(x3, spar = sp[j])+s(x4, spar = sp[j]),data = data_train)
    y_pred_test <- predict(model.tune[[j]], newdata = data_test)
    mse.tune[j] <-mean((y_pred_test-unlist(data_test['y']))^2)
    }, error = function(e) {
      cat("Error fitting model for spar", sp[j], ":", e$message, "\n")
      mse.tune[j] <- NA  # Assign NA for this model
    })
  }
  index <- which.min(mse.tune)
  model[[i]] <- model.tune[[index]]
  y_pred_train <- predict(model.tune[[index]], newdata = data_train)
  mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
  cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
  # y_pred_test <- predict(model, newdata = data_test)
  mse_test[i] <- mse.tune[index]
  cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
}




# mse_train <- numeric(nrow(he_train))
# mse_test <- numeric(nrow(he_train))
# model <- list()

# #spline
# for (i in 1:nrow(he_train)) {
#   data_train <- data.frame(
#     y = he_train[i, ],
#     x1 = gdp_train[i, ],
#     x2 = dr_young_train[i, ],
#     x3 = dr_old_train[i, ],
#     x4 = ghe_train[i, ]
#   )
#   data_test <- data.frame(
#     y = he_test[i, ],
#     x1 = gdp_test[i, ],
#     x2 = dr_young_test[i, ],
#     x3 = dr_old_test[i, ],
#     x4 = ghe_test[i, ]
#   )
#   model[[i]] <- gam( y ~ s(x1) + s(x2) + s(x3) + s (x4),data = data_train)
#   y_pred_train <- predict(model[[i]], newdata = data_train)
#   mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
#   cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
#   # y_pred_test <- predict(model, newdata = data_test)
#   y_pred_test <- predict(model[[i]], newdata = data_test)
#   mse_test[i] <- mean((unlist(data_test['y']) - y_pred_test)^2)
#   cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
# }


