# Create functions which pre-append fully-expanded file paths to relative paths
root <- is_rstudio_project$make_fix_file()
data_dir <- function(...) root("data", ...)
