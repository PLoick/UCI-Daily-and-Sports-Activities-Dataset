#' kNN
#' 
#' This file contains functions which perform the model building
#' and hyperparameter tuning of the kNN models
#' using the `h2o` machine learning framework.

build_knn <- function(train, test){
  x_train <- subset(train, select = -c(activity))
  y_train <- train$activity
  x_test <- subset(test, select = -c(activity))
  y_test <- test$activity
  
  train_time <- system.time(
    model_knn <- tune.knn(x_train, y_train, k = c(1,3,5,7,9),
                          tunecontrol=tune.control(sampling="bootstrap"))
  )
    
  model_train <- knn.cv(x_train, y_train, k=model_knn$best.parameters, prob=FALSE)
  
  run_time <- system.time(
    model_test <- knn(x_train, x_test, y_train, k=model_knn$best.parameters)
  )
    
  cm_train <- confusionMatrix(y_train, model_train)
  cm_test <- confusionMatrix(y_test, model_test)
  
  list(parameter = model_knn$best.parameters,
       train_time = train_time,
       run_time = run_time,
       perf = NULL,
       ypred_train = model_train,
       ypred_test = model_test,
       cm_train = cm_train,
       cm_test = cm_test)
  
}