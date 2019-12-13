library(h2o)
h2o.init(startH2O = FALSE)
loan_csv <- "https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv"
data <- h2o.importFile(loan_csv)  # 163,987 rows x 15 columns
dim(data)
