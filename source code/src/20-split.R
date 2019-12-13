create_run_files <- function(files, num_segments) {
  files %>%
    group_by(activity, participant) %>%
    sample_n(num_segments)
}

create_training_files <- function(run_files, training_prop) {
  run_files %>%
    sample_frac(training_prop)
}


create_test_files <- function(run_files, training_files) {
  run_files %>%
    filter(!(file_num %in% (training_files %>%
                              ungroup() %>%
                              extract2("file_num"))))
}

create_participant_split <- function(files) {
  test_set_participant <- sample(unique(files$participant), size = 1)
  
  list(
    training_files = filter(files, participant != test_set_participant),
    test_files = filter(files, participant == test_set_participant)
  )
}
