#' Principal Component Analysis
#' 
#' This file contains functions which perform the feature reduction
#' process of the dataset with principal component analysis methods
#' using the `h2o` machine learning framework.


#' Training set PCA
#' 
#' @param train Dataframe containing the training dataset where the first
#' column is the target and all other columns are input features.
#' 
#' @param n_components Number of principal components to reduce the train dataframe to
#' 
#' @return list This function returns a list containing the transformed
#' training set and the PCA vector loadings to apply to the test set.
do_pca <- function(train, test, n_components, model_dir = "h2o_models") {
  # Exclude activity label & translate into h2o environment
  cols_wo_label <- setdiff(names(train), "activity")
  train_hex <- to_h2o(train, frame_name = deparse(substitute(train)))
  test_hex <- to_h2o(test, frame_name = deparse(substitute(test)))
  
  # Develop model and apply principal components to train dataframe
  pca_model <- h2o.prcomp(train_hex, x=cols_wo_label, k=n_components, transform = "STANDARDIZE")
  train_pca <- h2o.predict(pca_model, train_hex, num_pc = n_components)
  
  # Apply principal components to test dataframe
  test_pca <- h2o.predict(pca_model, newdata = test_hex, num_pc = n_components)
  
  # Merge test_pca with activity label
  test_pca <- cbind(test$activity, as.data.frame(test_pca))
  colnames(test_pca)[1] <- "activity"
  
  # Merge train_pca with activity label
  train_pca <- cbind(train$activity, as.data.frame(train_pca))
  colnames(train_pca)[1] <- "activity"
  
  # Save the model object to disk
  model_path <- h2o.saveModel(object = pca_model, path = root(model_dir), force = TRUE)
  # Combine train_pca with pca_model as function return
  pca_results = list("train" = train_pca, 
                     "test" = test_pca, "pca_model" = pca_model, "model_path" = model_path)
  return(pca_results)
}
