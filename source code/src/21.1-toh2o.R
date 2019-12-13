merge_features <- function(df_summary, df_fft, df_acf) {
  df_summary %>%
    left_join(df_fft) %>%
    left_join(df_acf)
}

add_extension_features <- function(df_final, df_newfeat) {
  left_join(df_final, df_newfeat)
}

build_model_set <- function(df_final, model_files) {
  df_final %>%
    ungroup() %>%
    mutate(activity = as.factor(activity)) %>%
    filter(file_num %in% model_files$file_num) %>%
    select(-c(participant, segment)) %>%
    column_to_rownames(var = "file_num")
}

to_h2o <- function(frame, frame_name = deparse(substitute(frame))) {
  options("h2o.use.data.table" = TRUE)
  
  h2o_frame <- try(h2o.getFrame(frame_name), silent = TRUE)
  if(inherits(h2o_frame, "try-error")) {
    h2o_frame <- as.h2o(frame, destination_frame = frame_name)
  }
  h2o_frame
}
