## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      fig.width=9, fig.height=6)

## ----package-load, warning=FALSE, message=FALSE--------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(forcats)
library(pROC)
library(caret)
library(magrittr)
library(kableExtra)
library(knitr)
library(dummies)
library(data.table)

library(rprojroot)
# Create functions which pre-append fully-expanded file paths to relative paths
root <- is_rstudio_project$make_fix_file()
data_dir <- function(...) root("data", ...)

## ----download-data, eval=FALSE-------------------------------------------
## ## Data Loading
## outfile <- "data.zip"
## outdir <- "raw"
## url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00256/"
## download.file(url = paste0(url, outfile), destfile = data_dir(outfile))
## files <- unzip(data_dir(outfile), exdir = data_dir(outdir))

## ----list-files----------------------------------------------------------
## Run this if the data folder is already unzipped
files <- list.files(data_dir("raw"), full.names = TRUE, recursive = TRUE)

## ------------------------------------------------------------------------
files_metadata <-
  files %>%
  tools::file_path_sans_ext() %>%
  str_match("data/a.*") %>%
  as_tibble() %>%
  separate(V1, c("data", "activity", "participant", "segment"), sep = "/") %>%
  select(-data) %>%
  mutate(file_num = seq_len(nrow(.)))

## ------------------------------------------------------------------------
#files_subset <- str_subset(files, "/s0\\d")
daily_sport_activity_data <- map(files, fread) %>%
  bind_rows(.id = "file_num") %>%
  mutate(file_num = as.integer(file_num))

## ------------------------------------------------------------------------
daily_sport_activity_data <-
  inner_join(files_metadata, daily_sport_activity_data, by = "file_num")

## ------------------------------------------------------------------------
# Get the attribute names
daily_sport_activity_data_names <- c("T_xacc","T_yacc","T_zacc","T_xgyro","T_ygyro","T_zgyro","T_xmag","T_ymag" ,"T_zmag","RA_xacc","RA_yacc","RA_zacc","RA_xgyro", "RA_ygyro","RA_zgyro","RA_xmag", "RA_ymag","RA_zmag","LA_xacc","LA_yacc", "LA_zacc","LA_xgyro","LA_ygyro","LA_zgyro","LA_xmag","LA_ymag" , "LA_zmag" , "RL_xacc", "RL_yacc", "RL_zacc" , "RL_xgyro" ,"RL_ygyro","RL_zgyro","RL_xmag","RL_ymag" ,"RL_zmag","LL_xacc","LL_yacc", "LL_zacc","LL_xgyro","LL_ygyro","LL_zgyro","LL_xmag","LL_ymag","LL_zmag")

## ------------------------------------------------------------------------
names(daily_sport_activity_data) <-
  c("activity", "participant", "segment", "file_num", daily_sport_activity_data_names)

print(str(daily_sport_activity_data))

readr::write_csv(daily_sport_activity_data, data_dir("daily_sport_activity_data_newfilename.csv"))

## ---- eval=FALSE---------------------------------------------------------
## daily_sport_activity_data %>%
##   group_by(activity, participant, segment, file_num) %>%
##   summarise_all(funs(
##     mean = mean(., na.rm = TRUE),
##     med = median(., na.rm = TRUE)
##     ))

