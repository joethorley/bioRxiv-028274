source("header.R")
source("model-lek.R")

files <- list.files("output/lek", pattern = "^data[_]\\d", full.names = TRUE)

data <- lapply(files, readRDS)

files %<>%
  str_replace("output/lek/data_", "") %>%
  str_replace("[.]rds", "")

names(data) <- files

analyses <- analyse(model, data = data)

saveRDS(analyses, "output/lek/analyses.rds")
