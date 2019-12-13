# This is where you set up your workflow plan,
# a data frame with the steps of your analysis.

data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00256/data.zip"

get_data <- drake_plan(
  # Check the last-modified timestamp of the remote UCI datasets.
  # If the timestamp is more recent than the version currently cached
  # then this will invalidate all the targets as required and download
  # the latest version.
  # timestamp_data = HEAD(data_url)$headers[["last-modified"]],
  
  # Pass the timestamps as dummy function arguments so that they're treated as dependencies
  files = download_dataset(data_url, download_data = FALSE),
  strings_in_dots = "literals"
)

# build_plots <- drake_plan(
#   density_plot = create_density_plot(census_train),
#   capital_plot = create_capital_plot(census_train),
#   nominal_plot_data = create_nominal_plot_data(census_train),
#   nominal_var_plot = create_nominal_var_plot(nominal_plot_data),
#   native_country_plot = create_native_country_plot(nominal_plot_data)
# )

split_dataset <- drake_plan(
  run_files = create_run_files(files, num_segments),
  
  # Split into training and test sets
  training_files = create_training_files(run_files, training_prop),
  test_files = create_test_files(run_files, training_files)
)

preprocess_dataset <- drake_plan(
  sensors = create_sensors_df(),
  
  daily_sport_activity_data = read_run_files(run_files, sensors),
  
  # Part 1
  df_summary = calc_summary_measures(daily_sport_activity_data),
  
  # Part 2
  df_fft = calc_fft2(daily_sport_activity_data, sensors),
  
  # Part 3
  df_acf = calc_acf2(daily_sport_activity_data, sensors),
  
  # Part 4
  df_newfeat = calc_newfeat(daily_sport_activity_data, sensors),
  
  df_final = merge_features(df_summary, df_fft, df_acf),
  df_final__ext = add_extension_features(df_final, df_newfeat),
  
  train = build_model_set(df_final, training_files),
  test = build_model_set(df_final, test_files),
  
  train__ext = build_model_set(df_final__ext, training_files),
  test__ext = build_model_set(df_final__ext, test_files),
)

dimension_reduction <- drake_plan(
  # PCA
  pca_results = do_pca(train, test, n_components = 30, model_dir = "h2o_models"),
  train_pca = pca_results$train,
  test_pca = pca_results$test,
  
  pca_results__ext = do_pca(train__ext, test__ext, n_components = 30, model_dir = "h2o_models"),
  train_pca__ext = pca_results__ext$train,
  test_pca__ext = pca_results__ext$test,
  
  # t-SNE
  tsne_results_2 = do_tsne(train, test, n_components = 2),
  tsne_results_6 = do_tsne(train, test, n_components = 6),
  
  train_tsne2 = tsne_results_2$train,
  test_tsne2 = tsne_results_2$test,
  
  train_tsne6 = tsne_results_6$train,
  test_tsne6 = tsne_results_6$test,
  
  tsne_results_2__ext = do_tsne(train__ext, test__ext, n_components = 2),
  tsne_results_6__ext = do_tsne(train__ext, test__ext, n_components = 6),
  
  train_tsne2__ext = tsne_results_2__ext$train,
  test_tsne2__ext = tsne_results_2__ext$test,
  
  train_tsne6__ext = tsne_results_6__ext$train,
  test_tsne6__ext = tsne_results_6__ext$test,
  
  strings_in_dots = "literals"
)


build_models <- drake_plan(
  # Logistic Regression
  # mod_lg = build_model_logistic_regression(train, test),
  # mod_lg__ext = build_model_logistic_regression(train__ext, test__ext),
  
  # mod_lg_pca = build_model_logistic_regression(train_pca__ext, test_pca__ext),
  # mod_lg_pca__ext = build_model_logistic_regression(train_pca__ext, test_pca__ext),
  
  # mod_lg_tsne6 = build_model_logistic_regression(train_tsne6, test_tsne6),
  # mod_lg_tsne6__ext = build_model_logistic_regression(train_tsne6__ext, test_tsne6__ext),
  
  # Random Forest
  # mod_rf = build_model_random_forest(train, test),
  # mod_rf__ext = build_model_random_forest(train__ext, test__ext),
  
  # mod_rf_pca = build_model_random_forest(train_pca__ext, test_pca__ext),
  # mod_rf_pca__ext = build_model_random_forest(train_pca__ext, test_pca__ext),
  
  # mod_rf_tsne6 = build_model_random_forest(train_tsne6, test_tsne6),
  # mod_rf_tsne6__ext = build_model_random_forest(train_tsne6__ext, test_tsne6__ext),
  
  # XRT
  # mod_xrt = build_model_extremely_randomised_trees(train, test),
  # mod_xrt__ext = build_model_extremely_randomised_trees(train__ext, test__ext),
  
  # mod_xrt_pca = build_model_extremely_randomised_trees(train_pca__ext, test_pca__ext),
  # mod_xrt_pca__ext = build_model_extremely_randomised_trees(train_pca__ext, test_pca__ext),
  
  # mod_xrt_tsne6 = build_model_extremely_randomised_trees(train_tsne6, test_tsne6),
  # mod_xrt_tsne6__ext = build_model_extremely_randomised_trees(train_tsne6__ext, test_tsne6__ext),
  
  # kNN
  model_knn_pca = build_knn(train_pca, test_pca),
  model_knn_pca__ext = build_knn(train_pca__ext, test_pca__ext),
  
  model_knn_tsne2 = build_knn(train_tsne2, test_tsne2),
  model_knn_tsne2__ext = build_knn(train_tsne2__ext, test_tsne2__ext),
  
  model_knn_tsne6 = build_knn(train_tsne6, test_tsne6),
  model_knn_tsne6__ext = build_knn(train_tsne6__ext, test_tsne6__ext),
  
  # XGBoost
  # mod_xg = build_model_xgboost(train, test),
  # mod_xg__ext = build_model_xgboost(train__ext, test__ext),
  
  # mod_xg_pca = build_model_xgboost(train_pca__ext, test_pca__ext),
  # mod_xg_pca__ext = build_model_xgboost(train_pca__ext, test_pca__ext),
  
  # mod_xg_tsne6 = build_model_xgboost(train_tsne6, test_tsne6),
  # mod_xg_tsne6__ext = build_model_xgboost(train_tsne6__ext, test_tsne6__ext),
  
  # automl_pca = build_automl(train_pca__ext, test_pca__ext),
  
  strings_in_dots = "literals"
)


build_grids <- drake_plan(
  
  grd_lr__ext_pt1 = build_grid_lr(train__ext, test__ext),
  grd_lr_pca__ext_pt1 = build_grid_lr(train_pca__ext, test_pca__ext),
  grd_lr_tsne6__ext_pt1 = build_grid_lr(train_tsne6__ext, test_tsne6__ext),
  
  grd_rf__ext_pt1 = build_grid_rf_stage1(train__ext, test__ext),
  grd_rf_pca__ext_pt1 = build_grid_rf_stage1(train_pca__ext, test_pca__ext),
  grd_rf_tsne6__ext_pt1 = build_grid_rf_stage1(train_tsne6__ext, test_tsne6__ext),

  grd_xrt__ext_pt1 = build_grid_xrt_stage1(train__ext, test__ext),
  grd_xrt_pca__ext_pt1 = build_grid_xrt_stage1(train_pca__ext, test_pca__ext),
  grd_xrt_tsne6__ext_pt1 = build_grid_xrt_stage1(train_tsne6__ext, test_tsne6__ext),
  
  grd_xg__ext_pt1 = build_grid_xgboost_stage1(train__ext, test__ext),
  grd_xg_pca__ext_pt1 = build_grid_xgboost_stage1(train_pca__ext, test_pca__ext),
  grd_xg_tsne6__ext_pt1 = build_grid_xgboost_stage1(train_tsne6__ext, test_tsne6__ext),
 
  grd_xg__pt1 = build_grid_xgboost_stage1(train, test), 
  
  
  grd_rf__ext_pt2 = build_grid_rf_stage2(train__ext, test__ext),
  grd_rf_pca__ext_pt2 = build_grid_rf_stage2(train_pca__ext, test_pca__ext),
  grd_rf_tsne6__ext_pt2 = build_grid_rf_stage2(train_tsne6__ext, test_tsne6__ext),
  
  grd_xrt__ext_pt2 = build_grid_xrt_stage2(train__ext, test__ext),
  grd_xrt_pca__ext_pt2 = build_grid_xrt_stage2(train_pca__ext, test_pca__ext),
  grd_xrt_tsne6__ext_pt2 = build_grid_xrt_stage2(train_tsne6__ext, test_tsne6__ext),
  
  grd_xg__ext_pt2 = build_grid_xgboost_stage2(train__ext, test__ext),
  grd_xg_pca__ext_pt2 = build_grid_xgboost_stage2(train_pca__ext, test_pca__ext),
  grd_xg_tsne6__ext_pt2 = build_grid_xgboost_stage2(train_tsne6__ext, test_tsne6__ext),
  
  strings_in_dots = "literals"
)

ensembles <- drake_plan(
  
  mod_nn_pca = readr::read_rds(data_dir("mod_nn_pca.Rds")),
  mod_nn_tsne = readr::read_rds(data_dir("mod_nn_tsne.Rds")),
  mod_knn_tsne = readr::read_rds(data_dir("mod_knn_tsne.Rds")),
  mod_nn_tsne_ext = readr::read_rds(data_dir("mod_nn_tsne_ext.Rds")),
  
  ensemble_data_train = build_model_ensemble_data_train(
    test, grd_lr__ext_pt1,
    grd_rf_tsne6__ext_pt1,
    grd_xrt_tsne6__ext_pt1,
    grd_xg__ext_pt1,
    mod_nn_tsne_ext, model_knn_tsne6__ext),
  
  ensemble_data_test = build_model_ensemble_data_test(
    test, grd_lr__ext_pt1,
    grd_rf_tsne6__ext_pt1,
    grd_xrt_tsne6__ext_pt1,
    grd_xg__ext_pt1,
    mod_nn_tsne_ext, model_knn_tsne6__ext),
  
  mod_ensemble = build_model_ensemble(ensemble_data_train, ensemble_data_test),
  
  strings_in_dots = "literals"
)

participant_test_set <- drake_plan(
  participant_split = create_participant_split(files),
  
  train__ext__pcpt = build_model_set(df_final__ext, participant_split$training_files),
  test__ext__pcpt = build_model_set(df_final__ext, participant_split$test_files),
  
  tsne_results_6__ext__pcpt = do_tsne(train__ext__pcpt, test__ext__pcpt, n_components = 6),
  
  train_tsne6__ext__pcpt = tsne_results_6__ext__pcpt$train,
  test_tsne6__ext__pcpt = tsne_results_6__ext__pcpt$test,
  
  mod_automl_tsne6__ext__pcpt = build_auto_ml(train_tsne6__ext__pcpt, test_tsne6__ext__pcpt),
  grd_xg__ext__pcpt_pt1 = build_grid_xgboost_stage1(train_tsne6__ext__pcpt, test_tsne6__ext__pcpt),
  
  strings_in_dots = "literals"
)

knn_top_thousand <- drake_plan(
  top_1000_vars = grd_rf__ext_pt1$model@model$variable_importances$variable[1:1000],
  
  train__ext_1000 = subset(train__ext, select = top_1000_vars),
  test__ext_1000 = subset(test__ext, select = top_1000_vars),
  
  tsne_results_6__ext_1000 = do_tsne(train__ext_1000, test__ext_1000, n_components = 6),
  
  train_tsne6__ext_1000 = tsne_results_6__ext_1000$train,
  test_tsne6__ext_1000 = tsne_results_6__ext_1000$test,
  
  model_knn_tsne6__ext_1000 = build_knn(train_tsne6__ext_1000, test_tsne6__ext_1000)
)

project_plan <- rbind(
   get_data
  ,split_dataset
  ,preprocess_dataset
  ,dimension_reduction
  ,build_models
  ,build_grids
  ,ensembles
  ,participant_test_set
  ,knn_top_thousand
)
