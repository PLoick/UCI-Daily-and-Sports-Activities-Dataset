#' Model Ensembles
#' 
#' This file contains functions which perform the model building
#' and hyperparameter tuning of the stacking and ensemble methods
#' using a number of state-of-the-art R packages.

# 




build_model_ensemble <- function(train_ens_pred, test_ens_pred){

  # Logistic regression
  mod_lr <- build_grid_lr(train_ens_pred, test_ens_pred)
  
  # Random forest
  mod_rf <- build_grid_rf_ensemble(train_ens_pred, test_ens_pred)
  
  
  list(mod_lr = mod_lr,
       mod_rf = mod_rf)
  
}

build_auto_ml <- function(train, test) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  y <- "activity"
  x <- setdiff(names(train.hex), y)
  
  train_time <- system.time(
    aml <- h2o.automl(
      y = y, x = x,
      training_frame = train.hex,
      leaderboard_frame = test.hex,
      max_runtime_secs = 8*60*60, # eight hours
      seed = 101
    )
  )
  
  # Model leaderboard
  lb <- aml@leaderboard
  
  # Get model ids for all models in the AutoML Leaderboard
  model_ids <- as.data.frame(lb$model_id)[,1]
  
  # Get the best model regardless of ensemble or base learner
  model.best <- aml@leader
  # Get the "All Models" Stacked Ensemble model
  model.ensemble_all <- h2o.getModel(grep("StackedEnsemble_AllModels", 
                                          model_ids, value = TRUE)[1])
  # Get the "Best of Family" Stacked Ensemble model
  model.ensemble_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", 
                                             model_ids, value = TRUE)[1])
  
  # Get the Stacked Ensemble metalearner model
  metalearner.ensemble_all <- h2o.getModel(model.ensemble_all@model$metalearner$name)
  varimp.ensemble_all <- h2o.varimp(metalearner.ensemble_all)
  
  metalearner.ensemble_family <- h2o.getModel(model.ensemble_family@model$metalearner$name)
  varimp.ensemble_family <- h2o.varimp(metalearner.ensemble_family)
  
  perf.best <- h2o.performance(model.best, newdata = test.hex)
  perf.ensemble_all <- h2o.performance(model.ensemble_all, newdata = test.hex)
  perf.ensemble_family <- h2o.performance(model.ensemble_family, newdata = test.hex)
  
  ypred_train.best <- as.data.frame(h2o.predict(model.best, train.hex))
  
  run_time.best <- system.time(
    ypred_test.best <- as.data.frame(h2o.predict(model.best, test.hex))
  )
  
  ypred_train.ensemble_all <- as.data.frame(h2o.predict(model.ensemble_all, train.hex))
  
  run_time.ensemble_all <- system.time(
    ypred_test.ensemble_all <- as.data.frame(h2o.predict(model.ensemble_all, test.hex))
  )
  
  ypred_train.ensemble_family <- as.data.frame(h2o.predict(model.ensemble_family, train.hex))
  
  run_time.ensemble_family <- system.time(
    ypred_test.ensemble_family <- as.data.frame(h2o.predict(model.ensemble_family, test.hex))
  )
  
  list(
    lb = lb,
    
    model = model.best,
    model.ensemble_all = model.ensemble_all,
    model.ensemble_family = model.ensemble_family,
    
    ypred_train = ypred_train.best,
    ypred_train.ensemble_all = ypred_train.ensemble_all,
    ypred_train.ensemble_family = ypred_train.ensemble_family,
    
    ypred_test = ypred_test.best,
    ypred_test.ensemble_all = ypred_test.ensemble_all,
    ypred_test.ensemble_family = ypred_test.ensemble_family,
    
    train_time = train_time,
    
    run_time = run_time.best,
    run_time.ensemble_all = run_time.ensemble_all,
    run_time.ensemble_family = run_time.ensemble_family,
    
    perf = perf.best,
    perf.ensemble_all = perf.ensemble_all,
    perf.ensemble_family = perf.ensemble_family,
    
    metalearner.ensemble_all = metalearner.ensemble_all,
    varimp.ensemble_all = varimp.ensemble_all,
    
    metalearner.ensemble_family = metalearner.ensemble_family,
    varimp.ensemble_family = varimp.ensemble_family
  )
}

build_model_ensemble_data_train <- function(
  train, grd_lr__ext_pt1,
  grd_rf_tsne6__ext_pt1,
  grd_xrt_tsne6__ext_pt1,
  grd_xg__ext_pt1,
  mod_nn_tsne_ext, model_knn_tsne6__ext)
{
  data.frame(
    activity = train$activity,
    lr = grd_lr__ext_pt1$ypred_train$predict,
    rf = grd_rf_tsne6__ext_pt1$ypred_train$predict,
    xrt = grd_xrt_tsne6__ext_pt1$ypred_train$predict,
    xg = grd_xg__ext_pt1$ypred_train$predict,
    nn = as.factor(mod_nn_tsne_ext$nnpred_train),
    knn = as.factor(model_knn_tsne6__ext$ypred_train)
  )
}

build_model_ensemble_data_test <- function(
  test, grd_lr__ext_pt1,
  grd_rf_tsne6__ext_pt1,
  grd_xrt_tsne6__ext_pt1,
  grd_xg__ext_pt1,
  mod_nn_tsne_ext, model_knn_tsne6__ext)
{
  data.frame(
    activity = test$activity,
    lr = grd_lr__ext_pt1$ypred_test$predict,
    rf = grd_rf_tsne6__ext_pt1$ypred_test$predict,
    xrt = grd_xrt_tsne6__ext_pt1$ypred_test$predict,
    xg = grd_xg__ext_pt1$ypred_test$predict,
    nn = as.factor(mod_nn_tsne_ext$nnpred_test),
    knn = as.factor(model_knn_tsne6__ext$ypred_test)
  )
}

build_model_ensemble_h2o <- function(train, test){
  # Identify predictors and response
  y <- "activity"
  x <- setdiff(names(train), y)
  
  train.hex <- to_h2o(train, "train_stk")
  test.hex <- to_h2o(test, "test_stk")
  
  # 1. Generate a 3-model ensemble (GLM + RF + XGB)
  # Note: All base models must have the same cross-validation folds and
  # the cross-validated predicted values must be kept.
  
  # Train models using the optimal parameters from each respective grid-search
  
  # GLM
  glm_best <- h2o.gbm(y = "activity",
                      x = setdiff(names(train), y),
                      training_frame = train.hex,
                      distribution = "multinomial",
                      
                      # Set the hyperparameters to the best model from both grid searches
                      ntrees = 10,
                      max_depth = 3,
                      min_rows = 2,
                      learn_rate = 0.2,
                      
                      nfolds = 5,
                      fold_assignment = "Modulo", # Use the same folds across all base-learners
                      keep_cross_validation_predictions = TRUE, 
                      # Feed all predictions to super-learner
                      seed = 101)

  # RF
  rf_best <- h2o.randomForest(x = x,
                              y = y,
                              training_frame = train.hex,
                              
                              # Set the hyperparameters to the best model from both grid searches
                              ntrees = 50,
                              
                              nfolds = 5,
                              fold_assignment = "Modulo", 
                              # Use the same folds across all base-learners
                              keep_cross_validation_predictions = TRUE, 
                              # Feed all predictions to super-learner
                              seed = 101)
  
  # XGBoost
  xgb_best <- h2o.xgboost()
  
  # Train a stacked ensemble using the three models above
  ensemble <- h2o.stackedEnsemble(x = x,
                                  y = y,
                                  training_frame = train.hex,
                                  model_id = "h2o_ensemble",
                                  base_models = list(glm_best, rf_best, xgb_best))
  
  # Eval ensemble performance on a test set
  perf_ensemble <- h2o.performance(ensemble, newdata = test.hex)
  
  # Compare to base learner performance on the test set
  perf_glm <- h2o.performance(glm_best, newdata = test.hex)
  perf_rf <- h2o.performance(rf_best, newdata = test.hex)
  perf_xgb <- h2o.performance(xgb_best, newdata = test.hex)
  
  # Generate predictions on a test set
  pred <- h2o.predict(ensemble, newdata = test.hex)
}

build_grid_rf_ensemble <- function(train, test, ...) {
  train.hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test.hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  y <- "activity"
  x <- setdiff(names(train), y)
  
  hyper_params <- list(ntrees = round(10^seq(1, 4, 0.05)), 
                       # Test exponentially increasing sequence of n_trees
                       col_sample_rate_change_per_level = c(0.5, 1, 1.5),
                       col_sample_rate_per_tree = c(0.5, 1),
                       max_depth = seq(1, 40, 2))
  
  search_criteria <- list(strategy = "RandomDiscrete",
                          max_models = 50)
  
  train_time <- system.time(
    random.rf <- h2o.grid(
      algorithm = "randomForest",
      y = y, x = x,
      nfolds = 10,
      training_frame = train.hex,
      stopping_rounds = 2,
      stopping_tolerance = 0.000001,
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
