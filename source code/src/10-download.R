## Data Loading

download_dataset <- function(data_url, download_data = FALSE, ...) {
  if (download_data | (!dir.exists(data_dir("raw")))) {
    outfile <- basename(data_url)
    outdir <- "raw"
    download.file(url = data_url, destfile = data_dir(outfile))
    files <- unzip(data_dir(outfile), exdir = data_dir(outdir))
  } else {
    ## Run this if the data folder is already unzipped
    files <- list.files(data_dir("raw"), full.names = TRUE, recursive = TRUE)
  }
  file_hierarchy <- c("activity", "participant", "segment")
  tibble(
    path = files,
    file = str_remove_all(path, ".*/raw/data/") %>% tools::file_path_sans_ext()
  ) %>%
    separate(file, file_hierarchy, sep = "/") %>%
    mutate(file_num = seq_len(nrow(.)))
}
