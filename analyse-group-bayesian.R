source("header.R")
source("model-group-bayesian.R")

data <- readRDS("output/group/data_final.rds")

analysis <- analyse(model, data = data)

saveRDS(analysis, "output/group/analysis-bayesian.rds")
