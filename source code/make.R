## MAKEFILE

#' This file provides the build pipeline for reproducibly compiling the analysis
#' contained in this project. Since this pipeline was designed for a high performance
#' cluster environment, many of the models may take up to several days to build.
#' This can be circumvented by using the exported cache of results which reduces
#' the build time of the report to just a few seconds.

## Control Flow

# dataset_size_level <- 0 # Small, for debugging
#dataset_size_level <- 1 # Medium, for model development
dataset_size_level <- 2 # Full, for final results

if (dataset_size_level == 0) {
  # If debugging, sample 2 segments for every participant and activity
  num_segments <- 2
  training_prop <- 0.5
} else if (dataset_size_level == 1) {
  # If model developing, sample 9 segments
  num_segments <- 9
  training_prop <- 2/3
} else if (dataset_size_level == 2) {
  # If model tuning, sample all segments
  num_segments <- 60
  training_prop <- 2/3
} else {
  stop("Choose an appropriate training set size")
}

#n_components <- 30

## Setup

# More informative debugging
options(show.error.locations = TRUE,
        error = function() { traceback(1); if(!interactive()) quit("no", status = 1, runLast = FALSE) }
)

# Create a package installation helper function
install_if_not_already <- function(pkg, repo, method) {
  if(!(pkg %in% installed.packages()[,"Package"])) do.call(method, list(repo))
}

# Use the UK repo for CRAN
options(repos = c("CRAN" = "https://cran.ma.imperial.ac.uk"))

# Create functions which pre-append fully-expanded file paths to relative paths
install_if_not_already("rprojroot", "rprojroot", install.packages)
library(rprojroot)
root <- is_rstudio_project$make_fix_file()
data_dir <- function(...) root("data", ...)
src_dir <- function(...) root("src", ...)

# Install latest development versions of CodeDepends and drake
install_if_not_already("devtools", "devtools", install.packages)
install_if_not_already("graph", "http://callr.org/biocLite#graph", source)
install_if_not_already("CodeDepends", "duncantl/CodeDepends", devtools::install_github)
install_if_not_already("drake", "ropensci/drake", devtools::install_github)
install_if_not_already("tsfeatures", "robjhyndman/tsfeatures", devtools::install_github)

# Install all remaining packages used in src/01-packages.R from CRAN
pkgs <- gsub("^library\\(|\\)", "", grep("library", readLines(src_dir("01-packages.R")), value = TRUE), perl = TRUE)
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Upgrade stringr
if(packageVersion("stringr") < "1.3.0") devtools::install_github("tidyverse/stringr")

## Import Code

# Helper function to source src files
import_code <- function(r_file) source(src_dir(r_file))

# Load all packages
import_code("01-packages.R")

# Data Understanding
import_code("10-download.R")
# import_code("11-descriptive_stats.R")
# import_code("12-heatmaps.R")
# import_code("13-periodicity.R")

# Data Preparation
import_code("20-split.R")
import_code("21-preprocess.R")
import_code("21.1-toh2o.R")
import_code("22-pca.R")
import_code("23-tsne.R")

# Modelling
import_code("30-model_lr.R")
import_code("31-model_xgb.R")
import_code("33-model_knn.R")
# import_code("32-model_nn.R")
import_code("34-model_ensembles.R")
import_code("35-model_xrt.R")
import_code("36-model_rf.R")
# import_code("35-model_auto.R")

# Workflow
import_code("99-plan.R")

# Check for circularities, missing input files, etc.
check_plan(project_plan)

## Runtime

# Build the project
# this_job_cache <- drake::default_cache_path()
# if (Sys.getenv("JOB_ID") != "") {
#   this_job_cache <- this_cache("cache114179")
#   # this_job_cache <- new_cache(path = paste0("cache", Sys.getenv("JOB_ID")))
# } else {
#   this_job_cache <- drake::get_cache()
# }

future::plan(multicore, workers = 3)
#future::plan(sequential)
doFuture::registerDoFuture()

h2o.init(startH2O = FALSE)

this_job_cache <- this_cache(path = "cache_lr_rf_xrt_grids")
# this_job_cache <- this_cache(path = "small_build")
make(project_plan, parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)

### Part 1
# make(project_plan, targets =
#        c("grd_lr__ext_pt1", "grd_lr_pca__ext_pt1", "grd_lr_tsne6__ext_pt1"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)
# 
# make(project_plan, targets =
#        c("grd_rf__ext_pt1", "grd_rf_pca__ext_pt1", "grd_rf_tsne6__ext_pt1"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)
# 
# make(project_plan, targets =
#        c("grd_xrt__ext_pt1", "grd_xrt_pca__ext_pt1", "grd_xrt_tsne6__ext_pt1"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)
# 
# make(project_plan, targets =
#        c("grd_xg__ext_pt1", "grd_xg_pca__ext_pt1", "grd_xg_tsne6__ext_pt1"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)

### Part 2
# make(project_plan, targets =
#        c("grd_rf__ext_pt2", "grd_rf_pca__ext_pt2", "grd_rf_tsne6__ext_pt2"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)
# 
# make(project_plan, targets =
#        c("grd_xrt__ext_pt2", "grd_xrt_pca__ext_pt2", "grd_xrt_tsne6__ext_pt2"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)
# 
# make(project_plan, targets =
#        c("grd_xg__ext_pt2", "grd_xg_pca__ext_pt2", "grd_xg_tsne6__ext_pt2"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)

### Ensembles
# make(project_plan, targets =
#        c("mod_automl_tsne6__ext__pcpt"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)

# make(project_plan, targets =
#        c("train_pca", "grd_lr__ext_pt1", "grd_rf_tsne6__ext_pt1", "grd_xrt_tsne6__ext_pt1",
#          "grd_xg__ext_pt1", "mod_nn_tsne", "mod_knn_tsne", "mod_ensemble"),
#      parallelism = "future_lapply", cache = this_job_cache, seed = 101, keep_going = T)

# make(project_plan, targets =
#        c("mod_ensemble"),
