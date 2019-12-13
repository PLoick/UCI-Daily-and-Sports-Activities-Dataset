#' Random Forest
build_model_random_forest <- function(train, test) {
  train.hex <- to_h2o(train, "train_rf")
  test.hex <- to_h2o(test, "test_rf")
  
  train_time <- system.time(
    
    rf2 <- h2o.randomForest(              
      y=1,   
      training_frame = train.hex, 
      model_id = "rf_ActivitySport_v2",  
      ntrees = 200,                       
      max_depth = 30,                     
      stopping_rounds = 2,                 
      score_each_iteration = T,            
      seed = 111)                      
    
  )

  perf<-h2o.performance(rf2)
  varimp=h2o.varimp(rf2)
  ypred_train<-as.data.frame(h2o.predict(rf2,newdata = train.hex))
  
  run_time <- system.time(
    ypred_test<-as.data.frame(h2o.predict(rf2,newdata = test.hex))
  )
  
  perf <- h2o.performance(rf2, newdata = test.hex) 
  

  
 
  list(
    model = rf2
    ,train_time = train_time
    ,run_time = run_time
    ,perf = perf
    ,ypred_train = ypred_train
    ,ypred_test = ypred_test
    ,varimp = varimp
    
  )
}


build_grid_rf_stage1 <- function(train, test, ...) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
  hyper_params <- list(ntrees = round(10^seq(1, 4, 0.05)),
                       max_depth = seq(1, 40, 2))
  
  search_criteria <- list(strategy = "RandomDiscrete",
                          max_models = 200)
  
  train_time <- system.time(
    random.rf <- h2o.grid(
      algorithm = "randomForest",
      y = y, x = x,
      nfolds = 5,
      training_frame = train.hex,
      stopping_rounds = 2,
      stopping_tolerance = 0.01,
      score_each_iteration = T, 
      hyper_params = hyper_params,
      search_criteria = search_criteria,
      seed = 111
    )
  )
  
  # Retrieve the most accurate model
  grid <- h2o.getGrid(grid_id = random.rf@grid_id, sort_by = "accuracy", decreasing = TRUE)
  random.rf.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf <- h2o.performance(random.rf.best,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(random.rf.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(random.rf.best, test.hex))
  )
  
  list(
    grid = grid,
    model = random.rf.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}

#random forest hyperparameter grid search
build_grid_rf_stage2 <- function(train, test) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
  hyper_params <- list(ntrees = c(50,100,200,400,800,1600), 
                       max_depth = c(1, seq(3, 60, 3)))
  
  train_time <- system.time(
    grid.rf <- h2o.grid(
      algorithm = "randomForest",
      y = y, x = x,
      nfolds = 5,
      training_frame = train.hex,
      stopping_rounds = 2,
      stopping_tolerance = 0.01,
      
      score_each_iteration = T, 
      hyper_params = hyper_params,
      seed = 111
    )
  )
  
  # Retrieve the most accurate model
  grid <- h2o.getGrid(grid_id = grid.rf@grid_id, sort_by = "accuracy", decreasing = TRUE)
  grid.rf.best <- h2o.getModel(grid@summary_table[1, "model_ids"])
  
  perf <- h2o.performance(grid.rf.best,
                          newdata = test.hex)
  
  ypred_train <- as.data.frame(h2o.predict(grid.rf.best, train.hex))
  
  run_time <- system.time(
    ypred_test <- as.data.frame(h2o.predict(grid.rf.best, test.hex))
  )
  
  list(
    grid = grid.rf,
    model = grid.rf.best,
    ypred_train = ypred_train,
    ypred_test = ypred_test,
    train_time = train_time,
    run_time = run_time,
    perf = perf
  )
}
