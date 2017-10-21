source("header.R")
source("model-group-bayesian.R")

analysis <- readRDS("output/group/analysis.rds")

data <- data_set(analysis)

analysis <- analyse(model, data = data)

saveRDS(analysis, "output/group/analysis-bayesian.rds")
