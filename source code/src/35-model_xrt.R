#' Extremely Randomzed Trees
build_model_extremely_randomised_trees <- function(train, test) {
  train.hex <- to_h2o(train, "train_xrt")
  test.hex <- to_h2o(test, "test_xrt")  
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
#fitting model to do extremely_randomzed_trees 
  run_time <- system.time(
    xrt <- h2o.randomForest(               
      training_frame = train.hex,     
      y = y, x = x,
      nfolds = 5,
      model_id = "rf_ActivitySport_xrt",   
      ntrees = 200,                        
      max_depth = 30,                      
      stopping_rounds = 2,                 
      stopping_tolerance = 1e-2,    
      score_each_iteration = T,           
      histogram_type = "Random",
      seed = 111)                      
  )
  
#prediction in extremely_randomzed_trees
  ypred_train <- h2o.predict(xrt, newdata = train.hex)
  ypred_test <- h2o.predict(xrt, newdata = test.hex)
  
  perf <- h2o.performance(xrt, newdata = test.hex) 
  varimp <- h2o.varimp(xrt)
#extremely_randomzed_trees result-
  list(
     model = xrt
    ,run_time = run_time
    ,perf = perf
    ,ypred_train = ypred_train
    ,ypred_test = ypred_test
    ,varimp = varimp
  )
}

# grid search
build_grid_xrt_stage1 <- function(train, test, ...) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
# grid search "Cartesian"
  hyper_params <- list(ntrees = round(10^seq(1, 3, 0.05)), 
                          max_depth = seq(1, 40, 2))
  
  search_criteria <- list(strategy = "RandomDiscrete",
                               max_models = 200)
  
  train_time <- system.time(
    grid.xrt <- h2o.grid(
      algorithm = "randomForest",
      y = y, x = x,
      nfolds = 5,
      training_frame = train.hex,
      stopping_rounds = 2,
      stopping_tolerance = 0.01,
      score_each_iteration = T, 
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      histogram_type = "Random",
      seed = 111
    )
  )
  
# Retrieve the most accurate model
  grid <- h2o.getGrid(grid_id = grid.xrt@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.xrt.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf<- h2o.performance(grid.xrt.best,
                         newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.xrt.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.xrt.best, test.hex))
  )
  
  list(
    grid = grid,
    model = grid.xrt.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}

#extremely_randomzed_trees hyperparameter random search-

build_grid_xrt_stage2 <- function(train, test) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  hyper_params <- list(ntrees = c(50,100,200,400,800,1600),
                       max_depth = c(1, seq(3, 60, 3)))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
  train_time <- system.time(
    grid.xrt <- h2o.grid(
      algorithm = "randomForest",
      y = y, x = x,
      nfolds = 5,
      training_frame = train.hex,
      stopping_rounds = 2,
      stopping_tolerance = 0.01,
      score_each_iteration = T, 
      hyper_params = hyper_params,
      histogram_type = "Random",
      seed = 111
    )
  )
  
  # Retrieve the most accurate model for extremely_randomzed_trees
  grid <- h2o.getGrid(grid_id = grid.xrt@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.xrt.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf <- h2o.performance(grid.xrt.best,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.xrt.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.xrt.best, test.hex))
  )
  
  list(
    grid = grid,
    model = grid.xrt.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}
