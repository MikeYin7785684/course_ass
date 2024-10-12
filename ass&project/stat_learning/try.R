local_average <- function(xtrain, y, xtest=xtrain , bandwidth) {
  apply(xtest, 1, function(row) {
    weights <- exp(-rowSums((xtrain - row)^2) / (2 * bandwidth^2))
    sum(weights * y) / sum(weights)
  })
}
bw <- NULL
h <- 16
while (h>1e-5){
  bw<-c(bw,h/2)
  h<-h/2
}
tune_cv.local_average <- function(xtr,xtt,ytr,ytt,bw){
  mse_train.1 <- NULL
  mse_test.1 <- NULL
  for (h in bw){
    mse_train.1 <- c(mse_train.1,mean(local_average(xtr,ytr,xtr,h)))
    mse_test.1 <- c(mse_test.1,mean(local_average(xtr,ytt,xtt,h)))
  }
  return(list(mse_train=mse_train.1,mse_test=mse_test.1))
}

for (i in 1:nrow(he_train)) {
  data_train <- data.frame(
    y = he_train[i, ],
    x1 = gdp_train[i, ]
  )
  data_test <- data.frame(
    y = he_test[i, ],
    x1 = gdp_test[i, ]
  )
  
  # Check for sufficient data points
  if (nrow(data_train) < 5) {
    cat("Insufficient data for country:", country_name[i], "\n")
    next
  }
  
  s <- seq(0.5, 1, by=0.1)  # Adjusted span values
  model.tune <- vector("list", length(s))  
  mse.tune <- numeric(length(s))
  
  for (j in seq_along(s)) {
    cat("Trying span:", s[j], "\n")
    print(head(data_train))  # Check data structure
    
    model.tune[[j]] <- tryCatch(
      loess(y ~ x1, span=s[j], data=data_train),
      error=function(e) {
        cat("Error fitting model with span:", s[j], "\n")
        return(NULL)
      }
    )
    
    if (!is.null(model.tune[[j]])) {
      y_pred_test <- predict(model.tune[[j]], newdata=data_test)
      mse.tune[j] <- mean((y_pred_test - unlist(data_test['y']))^2, na.rm=TRUE)
    } else {
      mse.tune[j] <- Inf  # Assign a high MSE if model fitting fails
    }
  }
  
  # Check for valid models
  valid_indices <- which(!sapply(model.tune, is.null))  # Indices of valid models
  if (length(valid_indices) == 0) {
    cat("No valid models for country:", country_name[i], "\n")
    next
  }
  
  # Select the model with the minimum MSE among valid models
  index <- valid_indices[which.min(mse.tune[valid_indices])]
  model[[i]] <- model.tune[[index]]
  
  y_pred_train <- predict(model[[i]], newdata=data_train)
  mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2, na.rm=TRUE)
  cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
  
  mse_test[i] <- mse.tune[index]
  cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
}


mse_train <- numeric(nrow(he_train))
mse_test <- numeric(nrow(he_test))
model <- list()

#projection on gdp kernel local linear regression
for (i in 1:nrow(he_train)) {
  i<-1
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
  s <- seq(0.6,1,.1)
  model.tune <- list()  
  mse.tune <- numeric(length(s))
  for (j in seq_along(s)){
    j <-1
    model.tune[[j]] <- loess(y ~ x1+x2+x3 ,data = data_train,family = "gaussian")
    y_pred_test <- predict(model.tune[[j]], newdata = data_test)
    mse.tune[j] <-mean((y_pred_test-unlist(data_test['y']))^2)
  }
  print(min(mse.tune))
  index <- which.min(mse.tune)
  model[[i]] <- model.tune[[index]]
  y_pred_train <- predict(model.tune[[index]], newdata = data_train)
  mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
  cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
  # y_pred_test <- predict(model, newdata = data_test)
  mse_test[i] <- mse.tune[index]
  cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
}



i<-1
X_train <- rbind(
  gdp_train[i, ],
  dr_young_train[i, ],
  dr_old_train[i, ],
  ghe_train[i, ])
X_test <- rbind(
  gdp_test[i, ],
  dr_young_test[i, ],
  dr_old_test[i, ],
  ghe_test[i, ])
y_train <- he_train[i, ]
y_test <- he_test[i, ]
X_train <- t(X_train)
X_test <- t(X_test)


tune_cv.local_average(X_train,X_test,y_train,y_test,bw)

mse_train <- numeric(nrow(he_train))  # Initialize mse_train as a numeric vector
mse_test <- numeric(nrow(he_train))   # Initialize mse_test as a numeric vector
model <- list()  # Initialize model as a list

mse_train <- numeric(nrow(he_train))  # Initialize mse_train as a numeric vector
mse_test <- numeric(nrow(he_train))   # Initialize mse_test as a numeric vector
model <- list()  # Initialize model as a list

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
  
  s <- seq(0.1, 0.5, 0.1)  # Reduced range for span
  model.tune <- list()  # Use a list to store models
  mse.tune <- numeric(length(s))  # Initialize mse.tune as a numeric vector
  
  for (j in seq_along(s)) {  # Use seq_along for indexing
    # Try fitting the model with error handling
    tryCatch({
      model.tune[[j]] <- gam(y ~ lo(x1, span = s[j]), data = data_train)  # Start with one predictor
      # You can add more predictors gradually if needed
      y_pred_test <- predict(model.tune[[j]], newdata = data_test)
      mse.tune[j] <- mean((y_pred_test - unlist(data_test['y']))^2)
    }, error = function(e) {
      cat("Error fitting model for span", s[j], ":", e$message, "\n")
      mse.tune[j] <- NA  # Assign NA for this model
    })
  }
  
  index <- which.min(mse.tune, na.rm = TRUE)  # Get index of minimum MSE (ignoring NAs)
  
  # Store the best model if valid
  if (!is.na(index) && length(model.tune) > 0) {
    model[[i]] <- model.tune[[index]]  # Store the best model
  }
  
  # Ensure model is not NULL before predicting
  if (!is.null(model[[i]])) {
    y_pred_train <- predict(model[[i]], newdata = data_train)  # Use the best model
    mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
    
    cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
    mse_test[i] <- mse.tune[index]
    cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
  } else {
    cat("No valid model for country", country_name[i], "\n")
  }
}

mse_train <- numeric(nrow(he_train))  # Initialize mse_train
mse_test <- numeric(nrow(he_train))   # Initialize mse_test
model <- list()  # Initialize model list

mse_train <- numeric(nrow(he_train))  # Initialize mse_train
mse_test <- numeric(nrow(he_train))   # Initialize mse_test
model <- list()  # Initialize model list

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
  
  lambda <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  model.tune <- list()  
  mse.tune <- numeric(length(lambda))  # Initialize mse.tune for each iteration
  
  for (j in seq_along(lambda)) {
    # Fit the model with error handling
    tryCatch({
      model.tune[[j]] <- gam(y ~ s(x1) + 
                               s(x2) + 
                               s(x3) + 
                               s(x4),
                             data = data_train, 
                             control = gam.control(maxit = 100, trace = TRUE))
      # Set the lambda using the `gam.control`
      model.tune[[j]]$lambda <- lambda[j]
      
      y_pred_test <- predict(model.tune[[j]], newdata = data_test)
      mse.tune[j] <- mean((y_pred_test - unlist(data_test['y']))^2)
    }, error = function(e) {
      cat("Error fitting model for lambda", lambda[j], ":", e$message, "\n")
      mse.tune[j] <- NA  # Assign NA for this model
    })
  }
  
  # Get index of minimum MSE, ignoring NAs
  index <- which.min(mse.tune)
  
  # Store the best model if valid
  if (!is.na(index) && length(model.tune) > 0) {
    model[[i]] <- model.tune[[index]]
    
    # Predict on training data and calculate MSE
    y_pred_train <- predict(model[[i]], newdata = data_train)
    mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
    
    cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
    mse_test[i] <- mse.tune[index]
    cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
  } else {
    cat("No valid model for country", country_name[i], "\n")
  }
}

mse_train <- numeric(nrow(he_train))  # Initialize mse_train
mse_test <- numeric(nrow(he_train))   # Initialize mse_test
model <- list()  # Initialize model list

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
  
  spar_values <- seq(0.1, 1, by = 0.1)  # Define a range of spar values
  model.tune <- list()  
  mse.tune <- numeric(length(spar_values))  # Initialize mse.tune for each iteration
  
  for (j in seq_along(spar_values)) {
    # Fit the model with error handling
    tryCatch({
      model.tune[[j]] <- gam(y ~ s(x1, spar = spar_values[j]) + 
                               s(x2, spar = spar_values[j]) + 
                               s(x3, spar = spar_values[j]) + 
                               s(x4, spar = spar_values[j]),
                             data = data_train)
      
      y_pred_test <- predict(model.tune[[j]], newdata = data_test)
      mse.tune[j] <- mean((y_pred_test - unlist(data_test['y']))^2)
    }, error = function(e) {
      cat("Error fitting model for spar", spar_values[j], ":", e$message, "\n")
      mse.tune[j] <- NA  # Assign NA for this model
    })
  }
  
  # Get index of minimum MSE, ignoring NAs
  index <- which.min(mse.tune)
  
  # Store the best model if valid
  if (!is.na(index) && length(model.tune) > 0) {
    model[[i]] <- model.tune[[index]]
    
    # Predict on training data and calculate MSE
    y_pred_train <- predict(model[[i]], newdata = data_train)
    mse_train[i] <- mean((unlist(data_train['y']) - y_pred_train)^2)
    
    cat("Country:", country_name[i], "Training MSE:", mse_train[i], "\n")
    mse_test[i] <- mse.tune[index]
    cat("Country:", country_name[i], "Testing MSE:", mse_test[i], "\n")
  } else {
    cat("No valid model for country", country_name[i], "\n")
  }
}



