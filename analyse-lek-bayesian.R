source("header.R")
source("model-lek-bayesian.R")

data <- readRDS("output/lek/data_final.rds")

analysis <- analyse(model, data = data)

saveRDS(analysis, "output/lek/analysis-bayesian.rds")
