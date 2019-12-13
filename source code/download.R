library(dplyr)
library(magrittr)
# install.packages("boxr")
library(boxr)
library(rprojroot)
root <- is_rstudio_project$make_fix_file()
data_dir <- function(...) root("data", ...)

box_auth(client_id = "dv0o16na0mdp3nwzehtjlehirquidwwm", client_secret = "ybD8JkRpk2tu7O3cFZYZ4xGOfzdb5Nfm", interactive = FALSE, cache = ".boxr-oauth")

# Set the remote box_dir to caches
box_setwd("48514169932")

box_files <- boxr::box_ls() %>%
  as_tibble()

most_recent_cache <-
  box_files %>%
  arrange(desc(content_modified_at)) %>%
  top_n(1, wt = content_modified_at) %>%
  extract2("id")

# Download the most recent zipped cache
tar_cache <- box_dl(most_recent_cache, local_dir = root("caches"))
# tar_cache <- box_dl(most_recent_cache, local_dir = "/Users/willia76/Desktop")
# tar_cache <- box_dl(most_recent_cache, local_dir = "/Users/ploick")

# Uncompress the cache (this may take a while)
untar(tar_cache, exdir = root("caches"), compressed = "bzip2")
# untar(tar_cache, exdir = "/Users/willia76/Desktop", compressed = "bzip2")
# untar(tar_cache, exdir = "/Users/ploick", compressed = "bzip2")

tar_dir <- gsub(".tar.bz2", "", tar_cache, fixed = T)

library(drake)
current_results <- drake::this_cache(file.path(tar_dir, "drake"))
rm(box_files, most_recent_cache, tar_cache, tar_dir)
loadd(cache = current_results)

# Philipp code
# library(drake)
# current_results <- drake::this_cache(file.path("C:\\Users\\ploick\\caches\\180417_151029_xgboost_on_p_96dcc996", "drake"))
# current_results <- drake::this_cache(file.path("C:\\Users\\ploick\\caches\\180418_152714_misclassific_01731379", "drake"))
# loadd(cache = current_results)
