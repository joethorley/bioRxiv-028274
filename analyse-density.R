source("header.R")
source("model-density.R")

files <- list.files("output/density", full.names = TRUE)

data <- lapply(files, readRDS)

files %<>%
  str_replace("output/density/", "") %>%
  str_replace("[.]rds", "")

names(data) <- files

set_analysis_mode("quick")

analyses <- analyse(model_bayesian, data = data)

saveRDS(analyses, "output/density_analyses.rds")
