#' Neural Networks
#' 
#' This file contains functions which perform the model building
#' and hyperparameter tuning of the neural network models
#' using the `keras` machine learning framework.

build_neural_network <- function(train, test){
  source(root("src", "21-preprocess.R"))
  
  x_train <- subset(train, select = -c(activity))
  y_train <- subset(train, select = c(activity))
  x_test <- subset(test, select = -c(activity))
  y_test <- subset(test, select = c(activity))
  
  encoder <- onehot(y_train, max_levels=nrow(unique(y_train)))
  y_train_nn <- predict(encoder, y_train)
  y_test_nn <- predict(encoder, y_test)
  
  x_train_nn <- as.matrix(x_train)
  x_test_nn <- as.matrix(x_test)
  
  # Split training data into training and validation dataset 
  #- since we use validation accuracy to construct our model
  train_val_split <- train_test_split(x_train_nn, y_train_nn)
  x_train_train <- train_val_split$x_train
  x_train_val <- train_val_split$x_test
  y_train_train <- train_val_split$y_train
  y_train_val <- train_val_split$y_test
  
  nn <- keras_model_sequential()
  
  if(ncol(x_train_train) > 50){ # full features
    nn %>%
      layer_dense(units=500, activation = 'tanh', input_shape=ncol(x_train_nn)) %>%
      layer_dropout(rate = 0.1) %>% 
      layer_dense(units=250, activation = 'tanh', input_shape=ncol(x_train_nn)) %>%
      layer_dropout(rate = 0.1) %>% 
      layer_dense(units=100, activation = 'tanh') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units=50, activation = 'tanh') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = nrow(unique(y_train_nn)), activation = 'softmax')     
  } else if(ncol(x_train_train) > 6){ # PCA-reduce features
    nn %>%
      layer_dense(units=40, activation = 'tanh', input_shape=ncol(x_train_nn)) %>%
      layer_dropout(rate = 0.1) %>% 
      layer_dense(units=30, activation = 'tanh') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units=25, activation = 'tanh') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units=20, activation = 'tanh') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = nrow(unique(y_train_nn)), activation = 'softmax') 
  } else { # t-SNE-reduced features
    nn %>%
      layer_dense(units=40, activation = 'tanh', input_shape=ncol(x_train_nn)) %>%
      layer_dropout(rate = 0) %>% 
      layer_dense(units=30, activation = 'tanh') %>%
      layer_dropout(rate = 0) %>%
      layer_dense(units=25, activation = 'tanh') %>%
      layer_dropout(rate = 0) %>%
      layer_dense(units=20, activation = 'tanh') %>%
      layer_dropout(rate = 0) %>%
      layer_dense(units = nrow(unique(y_train_nn)), activation = 'softmax')     
  }

  nn %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  train_time <- system.time(
    nn_model <- nn %>% fit(
      x_train_train, y_train_train,
      epochs = 50, batch_size = 50,
      shuffle = TRUE,
      validation_data = list(x_train_val,y_train_val)
    )
  )
  
  visual <- plot(nn_model)
  
  # what is included in perf within xgb
  y_train_pred_nn <- nn %>% predict_classes(x_train_nn)
  
  run_time <- system.time(
    y_test_pred_nn <- nn %>% predict_classes(x_test_nn)
  )
  
  y_train_cat = create_num_categories(y_train)
  cm_train <- confusionMatrix(y_train_pred_nn+1, y_train_cat)
  
  y_test_cat = create_num_categories(y_test)
  cm_test <- confusionMatrix(y_test_pred_nn+1, y_test_cat)
  
  list(
    model = nn,
    train_time = train_time,
    run_time = run_time,
    perf = NULL,
    nnpred_train = y_train_pred_nn+1,
    nnpred_test = y_test_pred_nn+1,
    cm_train = cm_train,
    cm_test = cm_test,
    visual = visual
  )
}

build_neural_network_participant <- function(train, test){
  source(root("src", "21-preprocess.R"))
  
  x_train <- subset(train, select = -c(activity))
  y_train <- subset(train, select = c(activity))
  x_test <- subset(test, select = -c(activity))
  y_test <- subset(test, select = c(activity))
  
  encoder <- onehot(y_train, max_levels=nrow(unique(y_train)))
  y_train_nn <- predict(encoder, y_train)
  y_test_nn <- predict(encoder, y_test)
  
  x_train_nn <- as.matrix(x_train)
  x_test_nn <- as.matrix(x_test)
  
  # Split training data into training and validation dataset 
  #- since we use validation accuracy to construct our model
  train_val_split <- train_test_split(x_train_nn, y_train_nn)
  x_train_train <- train_val_split$x_train
  x_train_val <- train_val_split$x_test
  y_train_train <- train_val_split$y_train
  y_train_val <- train_val_split$y_test
  
  nn <- keras_model_sequential()
  
  nn %>%
    layer_dense(units=25, activation = 'tanh', input_shape=ncol(x_train_nn)) %>%
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units=20, activation = 'tanh') %>%
    layer_dropout(rate = 0.3) %>%
    #layer_dense(units=25, activation = 'tanh') %>%
    #layer_dropout(rate = 0) %>%
    #layer_dense(units=20, activation = 'tanh') %>%
    #layer_dropout(rate = 0) %>%
    layer_dense(units = nrow(unique(y_train_nn)), activation = 'softmax')     
  
  nn %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  train_time <- system.time(
    nn_model <- nn %>% fit(
      x_train_train, y_train_train,
      epochs = 50, batch_size = 50,
      shuffle = TRUE,
      validation_data = list(x_train_val,y_train_val)
    )
  )
  
  visual <- plot(nn_model)
  
  # what is included in perf within xgb
  y_train_pred_nn <- nn %>% predict_classes(x_train_nn)
  
  run_time <- system.time(
    y_test_pred_nn <- nn %>% predict_classes(x_test_nn)
  )
  
  y_train_cat = create_num_categories(y_train)
  cm_train <- confusionMatrix(y_train_pred_nn+1, y_train_cat)
  
  y_test_cat = create_num_categories(y_test)
  cm_test <- confusionMatrix(y_test_pred_nn+1, y_test_cat)
  
  list(
    model = nn,
    train_time = train_time,
    run_time = run_time,
    perf = NULL,
    nnpred_train = y_train_pred_nn+1,
    nnpred_test = y_test_pred_nn+1,
    cm_train = cm_train,
    cm_test = cm_test,
    visual = visual
  )
}

