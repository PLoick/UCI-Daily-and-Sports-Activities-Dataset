#' Linear Regression 
#' 
#' This file contains functions which perform the model building
#' and hyperparameter tuning of the LR models
#' using the `h2o` machine learning framework.

build_model_logistic_regression <- function(train, test) {
  train.hex <- to_h2o(train, "train_lr")
  test.hex <- to_h2o(test, "test_lr")
  y <- "activity"
  x <- setdiff(names(train), y)
  train_time <- system.time(
    train.glm <- h2o.glm(y = y, x = x,
                         training_frame = train.hex,
                         family = "multinomial",
                         nfolds = 5,
                         standardize = T,
                         lambda_search = F)
  )
  
  ypred_train <- as.data.frame(h2o.predict(train.glm, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(train.glm, test.hex))
  )

  perf = h2o.performance(train.glm, newdata = test.hex) 

  list(
     model = train.glm
    ,ypred_train = ypred_train
    ,ypred_test = ypred_test
    ,train_time = train_time
    ,run_time = run_time
    ,perf = perf
  )
} 

#logistic regression grid search
build_grid_lr <- function(train, test, ...) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  hyper_parameters = list(alpha = seq(0, 1, length.out = 11))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
  train_time <- system.time(
    grid.lgg <- h2o.grid("glm",
                         x = x,
                         y = y,
                         hyper_params = hyper_parameters,
                         training_frame = train.hex,
                         family = "multinomial",
                         nfolds = 5,
                         standardize = T,
                         lambda_search = T)
  )
  # Retrieve the most accurate model by grid search
  grid <- h2o.getGrid(grid_id = grid.lgg@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.lgg.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  
  perf <- h2o.performance(grid.lgg.best,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.lgg.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.lgg.best, test.hex))
  )
  
  list(
    grid = grid,
    model = grid.lgg.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}
