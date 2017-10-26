source("header.R")
source("model-lek-bayesian.R")

#data <- readRDS("output/lek/data_final.rds")

warning("need to get data_final")
data <- readRDS("output/lek/data_3200_1_1.rds")

analysis <- analyse(model, data = data)

saveRDS(analysis, "output/lek/analysis-bayesian.rds")
