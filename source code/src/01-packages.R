## Load all packages required for analysis.
## NOTE: Due to namespace conflicts, order matters.

# Tidyverse
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(tibble)
library(stringr)

# Preprocessing
library(data.table)
library(tsfeatures)
library(moments)
library(onehot)

# Modelling
library(caret)
library(h2o)
library(Rtsne)
library(keras)
# install_keras()
# keras installation guide: https://keras.rstudio.com/
library(class)
library(e1071)

# Workflow
library(knitr)
library(kableExtra)
library(rprojroot)
library(doFuture)
library(future.batchtools)
library(future)
library(drake)
