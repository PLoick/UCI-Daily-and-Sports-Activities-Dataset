#' t-stochastic neighbor embedding
#' 
#' This file contains functions which perform an alternative feature
#' reduction method next to principal component analysis. It is particularly
#' well-suited for visualizing

do_tsne <- function(train, test, n_components){
  total_data = rbind(train, test)
  train_wo_label <- subset(total_data, select = -c(activity))
  
  #set.seed(42)

  tsne_results = Rtsne(as.matrix(train_wo_label), dims = n_components, perplexity = 50, 
                       theta = 0.5, check_duplicates=FALSE, max_iter = 1000, pca_scale = FALSE)
  #tsne_results = Rtsne(as.matrix(train_wo_label), dims = n_components, perplexity = 50, 
  #theta = 0.5, check_duplicates=FALSE, max_iter = 1000, pca_scale = TRUE)
  
  tsne_results <- cbind(total_data$activity, as.data.frame(tsne_results$Y))
  colnames(tsne_results)[1] <- "activity"
  
  train_tsne <- tsne_results[1:nrow(train),]
  test_tsne <- tsne_results[(nrow(train)+1):nrow(total_data),]
  
  list(train = train_tsne,
       test = test_tsne,
       plot = plot(train_tsne[,2:3], col=train$activity)
  )
}
 

