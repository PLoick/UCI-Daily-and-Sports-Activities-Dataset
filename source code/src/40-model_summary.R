#' Model Summary
#' 
#' This file contains functions to summarize the results of the models

source(root("src", "21-preprocess.R"))

summary_table <- function(mod_full, mod_pca, mod_tsne, cap){
  
  summary <- tribble(
    ~Metrics, ~"Full Data", ~PCA, ~"t-SNE",
    "Accuracy - train", round(mod_full$cm_train$overall["Accuracy"],3), round(mod_pca$cm_train$overall["Accuracy"],3), round(mod_tsne$cm_train$overall["Accuracy"],3), 
    "Misclass Dens - train", norm_dens_train(mod_full), norm_dens_train(mod_pca), norm_dens_train(mod_tsne),
    "Weight Misclass Err - train", norm_err_train(mod_full), norm_err_train(mod_pca), norm_err_train(mod_tsne),
    "Training time, in sec", round(mod_full$train_time["elapsed"],1), round(mod_pca$train_time["elapsed"],1), round(mod_tsne$train_time["elapsed"],1), 
    "Accuracy - test", round(mod_full$cm_test$overall["Accuracy"],3), round(mod_pca$cm_test$overall["Accuracy"],3), round(mod_tsne$cm_test$overall["Accuracy"],3), 
    "Misclass Dens - test", norm_dens_test(mod_full), norm_dens_test(mod_pca), norm_dens_test(mod_tsne),
    "Weight Misclass Err - test", norm_err_test(mod_full), norm_err_test(mod_pca), norm_err_test(mod_tsne),
    "Running time, in sec", round(mod_full$run_time["elapsed"],1), round(mod_pca$run_time["elapsed"],1), round(mod_tsne$run_time["elapsed"],1), 
  )
  
  kable(summary, caption = cap)
  
}

# used for kNN, where there are 5 hyperparamaters tested
summary_table_pca_tsne <- function(mod_pca, mod_tsne, cap){
  
  summary <- tribble(
    ~Metrics, ~PCA, ~"t-SNE", 
    "Accuracy - train", round(mod_pca$cm_train$overall["Accuracy"],3), round(mod_tsne$cm_train$overall["Accuracy"],3), 
    "Misclass Dens - train", norm_dens_train(mod_pca), norm_dens_train(mod_tsne),
    "Weight Misclass Err - train", norm_err_train(mod_pca), norm_err_train(mod_tsne),
    "Training time, in sec", round(mod_pca$train_time["elapsed"]/5,1), round(mod_tsne$train_time["elapsed"]/5,1), 
    "Accuracy - test", round(mod_pca$cm_test$overall["Accuracy"],3), round(mod_tsne$cm_test$overall["Accuracy"],3), 
    "Misclass Dens - test", norm_dens_test(mod_pca), norm_dens_test(mod_tsne),
    "Weight Misclass Err - test", norm_err_test(mod_pca), norm_err_test(mod_tsne),
    "Running time, in sec", round(mod_pca$run_time["elapsed"],1), round(mod_tsne$run_time["elapsed"],1), 
  )
  
  kable(summary, caption = cap)
  
}

summary_table_h2o <- function(mod_full_pt1, mod_full_pt2, mod_pca_pt1, mod_pca_pt2, mod_tsne_pt1, mod_tsne_pt2, cap){
  
  acc_full_pt1_train = 1-h2o.mean_per_class_error(mod_full_pt1$model, train=TRUE)
  acc_full_pt1_test = 1-h2o.mean_per_class_error(mod_full_pt1$perf)
  acc_full_pt2_train = 1-h2o.mean_per_class_error(mod_full_pt2$model, train=TRUE)
  acc_full_pt2_test = 1-h2o.mean_per_class_error(mod_full_pt2$perf)
  
  acc_pca_pt1_train = 1-h2o.mean_per_class_error(mod_pca_pt1$model, train=TRUE)
  acc_pca_pt1_test = 1-h2o.mean_per_class_error(mod_pca_pt1$perf)
  acc_pca_pt2_train = 1-h2o.mean_per_class_error(mod_pca_pt2$model, train=TRUE)
  acc_pca_pt2_test = 1-h2o.mean_per_class_error(mod_pca_pt2$perf)
  
  acc_tsne_pt1_train = 1-h2o.mean_per_class_error(mod_tsne_pt1$model, train=TRUE)
  acc_tsne_pt1_test = 1-h2o.mean_per_class_error(mod_tsne_pt1$perf)
  acc_tsne_pt2_train = 1-h2o.mean_per_class_error(mod_tsne_pt2$model, train=TRUE)
  acc_tsne_pt2_test = 1-h2o.mean_per_class_error(mod_tsne_pt2$perf)
  
  summary_all <- tribble(
    ~Metrics, ~"Full Data (pt1)", ~"Full Data (pt2)", ~"PCA (pt1)", ~"PCA (pt2)", ~"t-SNE (pt1)", ~"t-SNE (pt2)", 
    "Accuracy - train", round(acc_full_pt1_train,3), round(acc_full_pt2_train,3), round(acc_pca_pt1_train,3), round(acc_pca_pt2_train,3), round(acc_tsne_pt1_train,3), round(acc_tsne_pt2_train,3), 
    "Misclass Dens - train", h2o_dens_train(mod_full_pt1), h2o_dens_train(mod_full_pt2), h2o_dens_train(mod_pca_pt1), h2o_dens_train(mod_pca_pt2), h2o_dens_train(mod_tsne_pt1), h2o_dens_train(mod_tsne_pt2),
    "Weight Misclass Err - train", h2o_err_train(mod_full_pt1), h2o_err_train(mod_full_pt2), h2o_err_train(mod_pca_pt1), h2o_err_train(mod_pca_pt2), h2o_err_train(mod_tsne_pt1), h2o_dens_train(mod_tsne_pt2),
    "Training time, in sec", round(mod_full_pt1$train_time["elapsed"]/length(mod_full_pt1$grid@model_ids),1), round(mod_full_pt2$train_time["elapsed"]/36,1), round(mod_pca_pt1$train_time["elapsed"]/length(mod_pca_pt1$grid@model_ids),1), round(mod_pca_pt2$train_time["elapsed"]/length(mod_pca_pt2$grid@model_ids),1), round(mod_tsne_pt1$train_time["elapsed"]/length(mod_tsne_pt1$grid@model_ids),1), round(mod_tsne_pt2$train_time["elapsed"]/length(mod_tsne_pt2$grid@model_ids),1),
    "Accuracy - test", round(acc_full_pt1_test,3), round(acc_full_pt2_test,3), round(acc_pca_pt1_test,3), round(acc_pca_pt2_test,3), round(acc_tsne_pt1_test,3), round(acc_tsne_pt2_test,3), 
    "Misclass Dens - test", h2o_dens_test(mod_full_pt1), h2o_dens_test(mod_full_pt2), h2o_dens_test(mod_pca_pt1), h2o_dens_test(mod_pca_pt2), h2o_dens_test(mod_tsne_pt1), h2o_dens_test(mod_tsne_pt2),
    "Weight Misclass Err - test", h2o_err_test(mod_full_pt1), h2o_err_test(mod_full_pt2), h2o_err_test(mod_pca_pt1), h2o_err_test(mod_pca_pt2), h2o_err_test(mod_tsne_pt1), h2o_err_test(mod_tsne_pt2),
    "Running time, in sec", round(mod_full_pt1$run_time["elapsed"],1), round(mod_full_pt2$run_time["elapsed"],1), round(mod_pca_pt1$run_time["elapsed"],1), round(mod_pca_pt2$run_time["elapsed"],1), round(mod_tsne_pt1$run_time["elapsed"],1), round(mod_tsne_pt2$run_time["elapsed"],1),
  )
  
  # checking mod_full
  if (acc_full_pt1_train > acc_full_pt2_train){
    mod_full = mod_full_pt1
    acc_full_train = acc_full_pt1_train
    acc_full_test = acc_full_pt1_test
  } else {
    mod_full = mod_full_pt2
    acc_full_train = acc_full_pt2_train
    acc_full_test = acc_full_pt2_test
  }
  
  # checking mod_pca
  if (acc_pca_pt1_train > acc_pca_pt2_train){
    mod_pca = mod_pca_pt1
    acc_pca_train = acc_pca_pt1_train
    acc_pca_test = acc_pca_pt1_test
  } else {
    mod_pca = mod_pca_pt2
    acc_pca_train = acc_pca_pt2_train
    acc_pca_test = acc_pca_pt2_test
  }
  
  # Checking mod_tsne
  if (acc_tsne_pt1_train > acc_tsne_pt2_train){
    mod_tsne = mod_tsne_pt1
    acc_tsne_train = acc_tsne_pt1_train
    acc_tsne_test = acc_tsne_pt1_test
  } else {
    mod_tsne = mod_tsne_pt2
    acc_tsne_train = acc_tsne_pt2_train
    acc_tsne_test = acc_tsne_pt2_test
  }

  summary <- tribble(
    ~Metrics, ~"Full Data", ~PCA, ~"t-SNE", 
    "Accuracy - train", round(acc_full_train,3), round(acc_pca_train,3), round(acc_tsne_train,3), 
    "Misclass Dens - train", h2o_dens_train(mod_full), h2o_dens_train(mod_pca), h2o_dens_train(mod_tsne),
    "Weight Misclass Err - train", h2o_err_train(mod_full), h2o_err_train(mod_pca), h2o_err_train(mod_tsne),
    "Training time, in sec", round(mod_full$train_time["elapsed"]/length(mod_full$grid@model_ids),1), round(mod_pca$train_time["elapsed"]/length(mod_pca$grid@model_ids),1), round(mod_tsne$train_time["elapsed"]/length(mod_tsne$grid@model_ids),1), 
    "Accuracy - test", round(acc_full_test,3), round(acc_pca_test,3), round(acc_tsne_test,3), 
    "Misclass Dens - test", h2o_dens_test(mod_full), h2o_dens_test(mod_pca), h2o_dens_test(mod_tsne),
    "Weight Misclass Err - test", h2o_err_test(mod_full), h2o_err_test(mod_pca), h2o_err_test(mod_tsne),
    "Running time, in sec", round(mod_full$run_time["elapsed"],1), round(mod_pca$run_time["elapsed"],1), round(mod_tsne$run_time["elapsed"],1), 
  )
  
  kable(summary, caption = cap)
  
}

summary_table_h2o_xg <- function(mod_full_pt1, mod_full_pt2, mod_pca_pt1, mod_pca_pt2, mod_tsne_pt1, mod_tsne_pt2, cap){
  
  acc_full_pt1_train = round(mod_full_pt1$cm_train$overall["Accuracy"],3)
  acc_full_pt1_test = 1-h2o.mean_per_class_error(mod_full_pt1$perf)
  acc_full_pt2_train = round(mod_full_pt2$cm_train$overall["Accuracy"],3)
  acc_full_pt2_test = 1-h2o.mean_per_class_error(mod_full_pt2$perf)
  
  acc_pca_pt1_train = round(mod_pca_pt1$cm_train$overall["Accuracy"],3)
  acc_pca_pt1_test = 1-h2o.mean_per_class_error(mod_pca_pt1$perf)
  acc_pca_pt2_train = round(mod_pca_pt2$cm_train$overall["Accuracy"],3)
  acc_pca_pt2_test = 1-h2o.mean_per_class_error(mod_pca_pt2$perf)
  
  acc_tsne_pt1_train = round(mod_tsne_pt1$cm_train$overall["Accuracy"],3)
  acc_tsne_pt1_test = 1-h2o.mean_per_class_error(mod_tsne_pt1$perf)
  acc_tsne_pt2_train = round(mod_tsne_pt2$cm_train$overall["Accuracy"],3)
  acc_tsne_pt2_test = 1-h2o.mean_per_class_error(mod_tsne_pt2$perf)
  
  # checking mod_full
  if (acc_full_pt1_train > acc_full_pt2_train){
    mod_full = mod_full_pt1
    acc_full_train = acc_full_pt1_train
    acc_full_test = acc_full_pt1_test
  } else {
    mod_full = mod_full_pt2
    acc_full_train = acc_full_pt2_train
    acc_full_test = acc_full_pt2_test
  }
  
  # checking mod_pca
  if (acc_pca_pt1_train > acc_pca_pt2_train){
    mod_pca = mod_pca_pt1
    acc_pca_train = acc_pca_pt1_train
    acc_pca_test = acc_pca_pt1_test
  } else {
    mod_pca = mod_pca_pt2
    acc_pca_train = acc_pca_pt2_train
    acc_pca_test = acc_pca_pt2_test
  }
  
  # Checking mod_tsne
  if (acc_tsne_pt1_train > acc_tsne_pt2_train){
    mod_tsne = mod_tsne_pt1
    acc_tsne_train = acc_tsne_pt1_train
    acc_tsne_test = acc_tsne_pt1_test
  } else {
    mod_tsne = mod_tsne_pt2
    acc_tsne_train = acc_tsne_pt2_train
    acc_tsne_test = acc_tsne_pt2_test
  }
  
  summary <- tribble(
    ~Metrics, ~"Full Data", ~PCA, ~"t-SNE", 
    "Accuracy - train", round(acc_full_train,3), round(acc_pca_train,3), round(acc_tsne_train,3), 
    "Misclass Dens - train", norm_dens_train(mod_full), norm_dens_train(mod_pca), norm_dens_train(mod_tsne),
    "Weight Misclass Err - train", norm_err_train(mod_full), norm_err_train(mod_pca), norm_err_train(mod_tsne),
    "Training time, in sec", round(mod_full$train_time["elapsed"]/length(mod_full$grid@model_ids),1), round(mod_pca$train_time["elapsed"]/length(mod_pca$grid@model_ids),1), round(mod_tsne$train_time["elapsed"]/length(mod_tsne$grid@model_ids),1), 
    "Accuracy - test", round(acc_full_test,3), round(acc_pca_test,3), round(acc_tsne_test,3), 
    "Misclass Dens - test", h2o_dens_test(mod_full), h2o_dens_test(mod_pca), h2o_dens_test(mod_tsne),
    "Weight Misclass Err - test", h2o_err_test(mod_full), h2o_err_test(mod_pca), h2o_err_test(mod_tsne),
    "Running time, in sec", round(mod_full$run_time["elapsed"],1), round(mod_pca$run_time["elapsed"],1), round(mod_tsne$run_time["elapsed"],1), 
  )
  
  kable(summary, caption = cap)
  
}

summary_table_h2o_one <- function(mod_rf, cap){

  acc_rf_train = 1-h2o.mean_per_class_error(mod_rf$model, train=TRUE)
  acc_rf_test = 1-h2o.mean_per_class_error(mod_rf$perf)
  
  summary <- tribble(
    ~Metrics, ~"RF",  
    "Accuracy - train", round(acc_rf_train,3),  
    "Misclass Dens - train", h2o_dens_train(mod_rf), 
    "Weight Misclass Dens - train", h2o_err_train(mod_rf), 
    "Training time, in sec", "n/a",  
    "Accuracy - test", round(acc_rf_test,3), 
    "Misclass Dens - test", h2o_dens_test(mod_rf), 
    "Weight Misclass Dens - test", h2o_err_test(mod_rf),
    "Running_time, in sec", round(mod_rf$run_time["elapsed"],1),  
  )
  
  kable(summary, caption = cap)
}


summary_table_all <- function(mod_lg, mod_rf, mod_xrt, mod_xg, mod_nn, mod_kNN, mod_ens, cap){
  
  acc_lg_train = 1-h2o.mean_per_class_error(mod_lg$model, train=TRUE)
  acc_lg_test = 1-h2o.mean_per_class_error(mod_lg$perf)
  
  acc_rf_train = 1-h2o.mean_per_class_error(mod_rf$model, train=TRUE)
  acc_rf_test = 1-h2o.mean_per_class_error(mod_rf$perf)
  
  acc_xrt_train = 1-h2o.mean_per_class_error(mod_xrt$model, train=TRUE)
  acc_xrt_test = 1-h2o.mean_per_class_error(mod_xrt$perf)
  
  acc_xg_train = mod_xg$cm_train$overall["Accuracy"]
  acc_xg_test = 1-h2o.mean_per_class_error(mod_xg$perf)
  
  acc_ens_train = 1-h2o.mean_per_class_error(mod_ens$model, train=TRUE)
  acc_ens_test = 1-h2o.mean_per_class_error(mod_ens$perf)
  
  summary <- tribble(
    ~Metrics, ~"LR (F)", ~"RF (SNE)", ~"XRT (SNE)", ~"XGB (F)", ~"NN (SNE)", ~"kNN (SNE)", ~"Ens (f)", 
    "Accuracy - train", round(acc_lg_train,3), round(acc_rf_train,3), round(acc_xrt_train,3), round(acc_xg_train,3), round(mod_nn$cm_train$overall["Accuracy"],3), round(mod_kNN$cm_train$overall["Accuracy"],3), round(acc_ens_train,3), 
    "Misclass Dens - train", h2o_dens_train(mod_lg), h2o_dens_train(mod_rf), h2o_dens_train(mod_xrt), norm_dens_train(mod_xg), norm_dens_train(mod_nn), norm_dens_train(mod_kNN), h2o_dens_train(mod_ens), 
    "Weight Misclass Dens - train", h2o_err_train(mod_lg), h2o_err_train(mod_rf), h2o_err_train(mod_xrt), norm_err_train(mod_xg), norm_err_train(mod_nn), norm_err_train(mod_kNN), h2o_err_train(mod_ens),
    "Training time, in sec", round(mod_lg$train_time["elapsed"]/length(mod_lg$grid@model_ids),1), round(mod_rf$train_time["elapsed"]/length(mod_rf$grid@model_ids),1), round(mod_xrt$train_time["elapsed"]/length(mod_xrt$grid@model_ids),1), round(mod_xg$train_time["elapsed"]/length(mod_xg$grid@model_ids),1), round(mod_nn$train_time["elapsed"],1), round(mod_kNN$train_time["elapsed"]/5,1), "n/a",  
    "Accuracy - test", round(acc_lg_test,3), round(acc_rf_test,3), round(acc_xrt_test,3), round(acc_xg_test,3), round(mod_nn$cm_test$overall["Accuracy"],3), round(mod_kNN$cm_test$overall["Accuracy"],3), round(acc_ens_test,3),
    "Misclass Dens - test", h2o_dens_test(mod_lg), h2o_dens_test(mod_rf), h2o_dens_test(mod_xrt), h2o_dens_test(mod_xg), norm_dens_test(mod_nn), norm_dens_test(mod_kNN), h2o_dens_test(mod_ens), 
    "Weight Misclass Dens - test", h2o_err_test(mod_lg), h2o_err_test(mod_rf), h2o_err_test(mod_xrt), h2o_err_test(mod_xg), norm_err_test(mod_nn), norm_err_test(mod_kNN), h2o_err_test(mod_ens),
    "Running_time, in sec", round(mod_lg$run_time["elapsed"],1), round(mod_rf$run_time["elapsed"],1), round(mod_xrt$run_time["elapsed"],1), round(mod_xg$run_time["elapsed"],1), round(mod_nn$run_time["elapsed"],1), round(mod_kNN$run_time["elapsed"],1), round(mod_ens$run_time["elapsed"],1), 
  )
  
  kable(summary, caption = cap)

}

# Misclassification Error

source(root("src", "41-cost_matrix.R"))

h2o_err_train <- function(h2o_model){
  temp = as.data.frame(h2o.confusionMatrix(h2o_model$model))
  temp = temp[1:19, 1:19]
  diag(temp)=0
  mc_p = sum(temp)
  temp = temp*as.data.frame(cost_matrix)[1:19, 2:20]
  if (mc_p == 0){
    return(0)
  } else {
    return(round(sum(temp)/mc_p, 0))
  }
  
}

h2o_err_test <- function(h2o_model){
  temp = as.data.frame(h2o.confusionMatrix(h2o_model$perf))
  temp = temp[1:19, 1:19]
  diag(temp)=0
  mc_p = sum(temp)
  temp = temp*as.data.frame(cost_matrix)[1:19, 2:20]
  if (mc_p == 0){
    return(0)
  } else {
    return(round(sum(temp)/mc_p, 0))
  }
}

norm_err_train <- function(norm_model){
  temp = norm_model$cm_train$table
  temp = temp[1:19, 1:19]
  diag(temp)=0
  mc_p = sum(temp)
  temp = as.vector(t(temp))
  temp = temp*as.data.frame(cost_matrix)[1:19, 2:20]
  if (mc_p == 0){
    return(0)
  } else {
    return(round(sum(temp)/mc_p, 0))
  }
  
}

norm_err_test <- function(norm_model){
  temp = norm_model$cm_test$table
  temp = temp[1:19, 1:19]
  diag(temp)=0
  mc_p = sum(temp)
  temp = as.vector(t(temp))
  temp = temp*as.data.frame(cost_matrix)[1:19, 2:20]
  if (mc_p == 0){
    return(0)
  } else {
    return(round(sum(temp)/mc_p, 0))
  }
  
}

# Misclassification Density

h2o_dens_train <- function(h2o_model){
  temp = as.data.frame(h2o.confusionMatrix(h2o_model$model))
  temp = temp[1:19, 1:19]
  diag(temp) = 0
  temp = as.vector(t(temp))
  return(round(sd(temp), 2))
  
}

h2o_dens_test <- function(h2o_model){
  temp = as.data.frame(h2o_model$perf@metrics$cm$table)
  temp = temp[1:19, 1:19]
  diag(temp) = 0
  temp = as.vector(t(temp))
  return(round(sd(temp), 2))
  
}

norm_dens_train <- function(norm_model){
  temp = norm_model$cm_train$table
  temp = temp[1:19, 1:19]
  temp = as.vector(t(temp))
  temp[temp >= 200] = 0
  return(round(sd(temp), 2))
  
}

norm_dens_test <- function(norm_model){
  temp = norm_model$cm_test$table
  temp = temp[1:19, 1:19]
  temp = as.vector(t(temp))
  temp[temp >= 100] = 0
  return(round(sd(temp), 2))
  
}

train_confusion_matrix <- function(h2o_model){
  a = create_num_categories(h2o_model$ypred_train["predict"])
  b = create_num_categories(train$activity)
  return(confusionMatrix(a, b))
}