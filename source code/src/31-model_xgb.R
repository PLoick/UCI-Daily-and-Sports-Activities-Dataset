#' XGBoost
#' 
#' This file contains functions which perform the model building
#' and hyperparameter tuning of the eXtreme Gradient Boosting model
#' using the `h2o` machine learning framework.

build_model_xgboost <- function(train, test) {
  train.hex <- to_h2o(train, "train_xgb")
  test.hex <- to_h2o(test, "test_xgb")
  
  train_time <- system.time(
    train.xgboost <- h2o.xgboost(
      y = "activity",
      training_frame = train.hex,
      nfolds = 5,
      backend = "cpu",
      
      # Hyperparameters
      ntrees = 200,
      max_depth = 20,
      learn_rate = 0.3,
      sample_rate = 0.75,
      col_sample_rate = 0.75,
      reg_lambda = 0.0, # L2 regularisation
      reg_alpha	= 0.0, # L1 regularisation
      
      seed = 101,
      distribution = "multinomial"
    )
  )
  
  perf <- h2o.performance(train.xgboost,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(train.xgboost, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(train.xgboost, test.hex))
  )
  
  list(
    model = train.xgboost,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}

build_grid_xgboost_stage1 <- function(train, test) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  hyper_params <- list(ntrees = round(10^seq(1, 3, 0.05)), 
                       # Test exponentially increasing sequence of n_trees
                       max_depth = seq(1, 30, 2),
                       learn_rate = 10^seq(-4, -0.5, 0.05), 
                       # Test exponentially increasing sequence of learning rates
                       sample_rate = seq(0.5, 1.0, 0.01),
                       col_sample_rate = seq(0.2, 1.0, 0.01),
                       reg_lambda = seq(0, 1.0, 0.1),
                       reg_alpha = seq(0, 1.0, 0.1))
  
  search_criteria <- list(strategy = "RandomDiscrete",
                          max_models = 20, max_runtime_secs = 7200,
                          seed = 101)
  
  train_time <- system.time(
    grid.xgboost <- h2o.grid(
      algorithm = "xgboost",
      x = setdiff(names(train.hex), "activity"),
      y = "activity",
      training_frame = train.hex,
      nfolds = 5,
      backend = "cpu",    
      
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      
      seed = 101
    )
  )
  
  # Retrieve the most accurate model
  grid <- h2o.getGrid(grid_id = grid.xgboost@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.xgboost.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf <- h2o.performance(grid.xgboost.best,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.xgboost.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.xgboost.best, test.hex))
  )
  
  list(
    grid = grid,
    model = grid.xgboost.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}

build_grid_xgboost_stage2 <- function(train, test) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
    
  hyper_params <- list(ntrees = c(500, 1000, 2000), 
                       # Test exponentially increasing sequence of n_trees
                       max_depth = c(3, 5, 10),
                       reg_lambda = c(0.2, 0.8),
                       reg_alpha = c(0.2, 0.8))
  
  train_time <- system.time(
    grid.xgboost <- h2o.grid(
      algorithm = "xgboost",
      x = setdiff(names(train.hex), "activity"),
      y = "activity",
      training_frame = train.hex,
      nfolds = 5,
      backend = "cpu",
      
      # Set some hyperparameters according to the optimal from Stage 1
      col_sample_rate = 0.81,
      sample_rate = 0.7,
      learn_rate = 0.08,
      
      hyper_params = hyper_params,
      
      seed = 101
    )
  )
  
  # Retrieve the most accurate model
  grid <- h2o.getGrid(grid_id = grid.xgboost@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.xgboost.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf <- h2o.performance(grid.xgboost.best, newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.xgboost.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.xgboost.best, test.hex))
  )
  
  list(
    grid = grid,
    model = grid.xgboost.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}
