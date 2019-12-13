create_sensors_df <- function() {
  sensor_names <- c("T_xacc","T_yacc","T_zacc","T_xgyro","T_ygyro","T_zgyro","T_xmag","T_ymag" ,
                    "T_zmag","RA_xacc","RA_yacc","RA_zacc","RA_xgyro", "RA_ygyro","RA_zgyro",
                    "RA_xmag", "RA_ymag","RA_zmag","LA_xacc","LA_yacc", "LA_zacc","LA_xgyro",
                    "LA_ygyro","LA_zgyro","LA_xmag","LA_ymag" , "LA_zmag" , "RL_xacc", "RL_yacc", 
                    "RL_zacc" , "RL_xgyro" ,"RL_ygyro","RL_zgyro","RL_xmag","RL_ymag" ,"RL_zmag",
                    "LL_xacc","LL_yacc", "LL_zacc","LL_xgyro","LL_ygyro","LL_zgyro","LL_xmag",
                    "LL_ymag","LL_zmag")
  sensors <- sensor_names %>%
    tibble(seq_along(.)) %>%
    set_colnames(c("sensor", "id"))
  sensors
}


read_run_files <- function(run_files, sensors) {
  paths <- extract2(run_files, "path")
  sensor_names <- sensors$sensor
  
  daily_sport_activity_data <- map(paths, fread) %>%
    rbindlist() %>%
    as_tibble() %>%
    cbind(tibble(path = rep(paths, each = 125))) %>%
    left_join(run_files)
  
  file_hierarchy <- c("activity", "participant", "segment")
  names(daily_sport_activity_data) <-
    c(sensor_names, "path", file_hierarchy, "file_num")
  daily_sport_activity_data %<>% select(c("file_num", file_hierarchy, sensor_names))
  
  daily_sport_activity_data
}

calc_summary_measures <- function(daily_sport_activity_data) {
  daily_sport_activity_data %>%
    group_by(activity, participant, segment, file_num) %>%
    summarise_all(funs(
      min = min(., na.rm = TRUE),
      max = min(., na.rm = TRUE),
      mean = mean(., na.rm = TRUE),
      skew = skewness,
      kurt = kurtosis
    ))
}

get_fft <- function(vec, num_peaks) {
  sr <- 25 # 25Hz sample frequency
  
  # Run the Fast Discrete Fourier Transform
  fourier <- fft(vec)
  # Calculate the strength of each peak
  fourier_peaks <- Mod(fourier)
  # Calculate the equivalent frequency of each peak
  fourier_freq <- (seq_along(vec) - 1) * (sr / length(vec))
  # Return the top num_peaks peaks and their frequencies
  top_peaks_ix <- sort(fourier_peaks, index.return = TRUE, decreasing = TRUE) %>%
  {head(.$ix, num_peaks)}
  list(
    fourier_peaks = fourier_peaks[top_peaks_ix],
    fourier_freq = fourier_freq[top_peaks_ix]
  )
}

do_fft <- function(daily_sport_activity_data, samples, sensors, num_peaks) {
  foreach(j = samples) %:%
    foreach(i = sensors$sensor) %dopar% {
      get_fft(subset(daily_sport_activity_data, file_num == j, i), num_peaks)
    }
}

calc_fft <- function(daily_sport_activity_data, sensors) {
  samples <- daily_sport_activity_data$file_num %>% unique()
  num_sensors <- nrow(sensors)
  
  num_peaks <- 5 # Return the Top 5 peaks only
  
  df_fft <-
    do_fft(daily_sport_activity_data, samples, sensors, num_peaks) %>%
    flatten_dfr(.id = "sensor") %>%
    group_by(sensor) %>%
    mutate(fourier_no = row_number()) %>%
    ungroup() %>%
    mutate(file_num = rep(samples, each = num_sensors * num_peaks),
           sensor = rep(seq_len(num_sensors), each = num_peaks, times = length(samples))) %>%
    left_join(sensors, by = c("sensor" = "id")) %>%
    tidyr::gather("key", "val", fourier_peaks, fourier_freq) %>%
    unite(key, sensor.y, key, fourier_no) %>%
    select(-sensor) %>%
    spread(key, val)
  
  df_fft
}

do_fft2 <- function(df, samples, sensors, num_peaks) {
  dt <- as.data.table(df)
  setkey(dt, file_num)
  dt_freq <- dt[, lapply(.SD, get_freq, num_peaks = num_peaks),
                by = file_num, .SDcols = sensors$sensor]
  dt_peaks <- dt[, lapply(.SD, get_peaks, num_peaks = num_peaks),
                 by = file_num, .SDcols = sensors$sensor]
  
  names(dt_freq) <- c("file_num",paste0(names(dt_freq)[-1], "_freq"))
  names(dt_peaks) <- c("file_num",paste0(names(dt_peaks)[-1], "_peaks"))
  
  dt_freq[, fourier_no := rowid(file_num)]
  dt_peaks[, fourier_no := rowid(file_num)]
  
  merge(dt_peaks, dt_freq, by = c("file_num", "fourier_no"))
}

get_freq <- function(vec, num_peaks) {
  sr <- 25 # 25Hz sample frequency
  
  # Run the Fast Discrete Fourier Transform
  fourier <- fft(vec)
  # Calculate the strength of each peak
  fourier_peaks <- Mod(fourier)
  # Calculate the equivalent frequency of each peak
  fourier_freq <- (seq_along(vec) - 1) * (sr / length(vec))
  # Return the top num_peaks peaks and their frequencies
  top_peaks_ix <- sort(fourier_peaks, index.return = TRUE, decreasing = TRUE) %>%
  {head(.$ix, num_peaks)}
  
  fourier_freq[top_peaks_ix]
}

get_peaks <- function(vec, num_peaks) {
  sr <- 25 # 25Hz sample frequency
  
  # Run the Fast Discrete Fourier Transform
  fourier <- fft(vec)
  # Calculate the strength of each peak
  fourier_peaks <- Mod(fourier)
  # Calculate the equivalent frequency of each peak
  fourier_freq <- (seq_along(vec) - 1) * (sr / length(vec))
  # Return the top num_peaks peaks and their frequencies
  top_peaks_ix <- sort(fourier_peaks, index.return = TRUE, decreasing = TRUE) %>%
  {head(.$ix, num_peaks)}
  
  fourier_peaks[top_peaks_ix]
}

calc_fft2 <- function(daily_sport_activity_data, sensors) {
  samples <- daily_sport_activity_data$file_num %>% unique()
  num_sensors <- nrow(sensors)
  
  num_peaks <- 5 # Return the Top 5 peaks only
  
  df_fft <-
    do_fft2(daily_sport_activity_data, samples, sensors, num_peaks) %>%
    melt(id.vars = c("file_num", "fourier_no"))
    # flatten_dfr(.id = "sensor") %>%
    # group_by(sensor) %>%
    # mutate(fourier_no = row_number()) %>%
    # ungroup() %>%
    # mutate(file_num = rep(samples, each = num_sensors * num_peaks),
    #        sensor = rep(seq_len(num_sensors), each = num_peaks, times = length(samples))) %>%
    # left_join(sensors, by = c("sensor" = "id")) %>%
    # tidyr::gather("key", "val", fourier_peaks, fourier_freq) %>%
  
  df_fft[, variable := paste(variable, fourier_no, sep = "_")]
  df_fft[, fourier_no := NULL]
  
  dcast(df_fft, file_num ~ variable, value.var = "value")
}

do_acf <- function(daily_sport_activity_data, samples, sensors, num_acf_samples) {
  foreach(j = samples) %:%
    foreach(i = sensors$sensor) %dopar% {
      stats::acf(
        daily_sport_activity_data,
        # subset(daily_sport_activity_data, file_num == j, i),
        lag.max = 200, plot = F, type = "covariance"
        )$acf[seq(from = 1, by = 5, length.out = num_acf_samples)]
    }
}

calc_acf <- function(daily_sport_activity_data, sensors) {
  samples <- daily_sport_activity_data$file_num %>% unique()
  
  num_acf_samples <- 11
  
  df_acf <-
    do_acf(daily_sport_activity_data, samples, sensors, num_acf_samples)
  rbindlist() %>% as_tibble() %>%
    mutate(file_num = rep(samples, each = num_acf_samples),
           acf_sample = rep(seq_len(num_acf_samples), times = length(samples))) %>%
    tidyr::gather("sensor", "value", -c(file_num, acf_sample)) %>%
    mutate(sensor = as.integer(gsub("V", "", sensor))) %>%
    left_join(sensors, by = c("sensor" = "id")) %>%
    unite(key, sensor.y, acf_sample, sep = "_acf_") %>%
    select(-sensor) %>%
    spread(key, value)
  
  df_acf
}


get_acf <- function(data, num_acf_samples) {
  stats::acf(
    data,
    lag.max = 200, plot = F, type = "covariance"
  )$acf[seq(from = 1, by = 5, length.out = num_acf_samples)]
}
do_acf2 <- function(df, sensors, num_acf_samples) {
  dt <- as.data.table(df)
  setkey(dt, file_num)
  dt[, lapply(.SD, get_acf, num_acf_samples = num_acf_samples),
     by = file_num, .SDcols = sensors$sensor]
}
calc_acf2 <- function(daily_sport_activity_data, sensors) {
  num_acf_samples <- 11
  
  df_acf <-
    do_acf2(daily_sport_activity_data, sensors, num_acf_samples) %>%
    as_tibble() %>%
    mutate(acf_sample = rep(seq_len(num_acf_samples), times = uniqueN(file_num))) %>%
    tidyr::gather("sensor", "value", -c(file_num, acf_sample)) %>%
    # mutate(sensor = as.integer(gsub("V", "", sensor))) %>%
    # left_join(sensors, by = c("sensor" = "id")) %>%
    unite(sensor, sensor, acf_sample, sep = "_acf_") %>%
    # select(-sensor) %>%
    spread(sensor, value)
  
  df_acf
}

get_new_features <- function(vec) {
  vec <- as.ts(vec)
  width <- ifelse(frequency(vec) > 1, frequency(vec), 25)
  
  level_shift = tsfeatures::max_level_shift(vec, width = width)
  var_shift = tsfeatures::max_var_shift(vec, width = width)
  lumpiness = tsfeatures::lumpiness(vec, width = width)
  stability = tsfeatures::stability(vec, width = width)
  stl_features = tsfeatures::stl_features(vec, s.window = "periodic", robust = TRUE)
  crossing_points = tsfeatures::crossing_points(vec)
  entropy = tsfeatures::entropy(vec)
  flat_spots = tsfeatures::flat_spots(vec)
  
  c(
    max_level_shift = level_shift[["max_level_shift"]],
    time_level_shift = level_shift[["time_level_shift"]],
    max_var_shift = var_shift[["max_var_shift"]],
    time_var_shift = var_shift[["time_var_shift"]],
    
    lumpiness = lumpiness[["lumpiness"]],
    stability = stability[["stability"]],
    
    nperiods = stl_features[["nperiods"]],
    seasonal_period = stl_features[["seasonal_period"]],
    trend = stl_features[["trend"]],
    spike = stl_features[["spike"]],
    linearity = stl_features[["linearity"]],
    curvature = stl_features[["curvature"]],
    
    crossing_points = crossing_points[["crossing_points"]],
    entropy = entropy[["entropy"]],
    flat_spots = flat_spots[["flat_spots"]]
  )
}

do_newfeat <- function(df, sensors) {
  dt <- as.data.table(df)
  setkey(dt, file_num)
  dt[, lapply(.SD, get_new_features), by = file_num, .SDcols = sensors$sensor]
}

calc_newfeat <- function(daily_sport_activity_data, sensors) {
  new_feats <-
    c("max_level_shift","time_level_shift","max_var_shift","time_var_shift",
      "lumpiness","stability","nperiods","seasonal_period",
      "trend","spike","linearity","curvature",
      "crossing_points","entropy","flat_spots")
  
  df_newfeat <-
    do_newfeat(daily_sport_activity_data, sensors) %>%
    as_tibble() %>%
    mutate(feature = rep(new_feats, times = uniqueN(file_num))) %>%
    tidyr::gather("sensor", "value", -c(file_num, feature)) %>%
    unite(sensor, sensor, feature, sep = "_") %>%
    spread(sensor, value)
  
  df_newfeat
}

create_num_categories <- function(df){
  df = as.factor(as.matrix(df))
  levels(df) = 1:length(levels(df))
  df = as.numeric(df)
  return(df)
}

train_test_split <- function(x_train, y_train){
  smp_size <- floor(0.75 * nrow(x_train))
  # set.seed(42)
  train_ind <- sample(seq_len(nrow(x_train)), size = smp_size)
  
  x_train <- x_train[train_ind, ]
  x_test <- x_train[-train_ind, ]
  
  y_train <- y_train[train_ind, ]
  y_test <- y_train[-train_ind, ]
  
  list(x_train = x_train,
       x_test = x_test,
       y_train = y_train,
       y_test = y_test)
}
