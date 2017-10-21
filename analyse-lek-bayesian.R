source("header.R")
source("model-lek-bayesian.R")

analysis <- readRDS("output/lek/analysis.rds")

data <- data_set(analysis)

analysis <- analyse(model, data = data) %>%
  reanalyse()

saveRDS(analysis, "output/lek/analysis-bayesian.rds")
