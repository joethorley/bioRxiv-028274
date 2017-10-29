source("header.R")
source("model-group.R")

files <- list.files("output/group", pattern = "^data[_]\\d", full.names = TRUE)

data <- lapply(files, readRDS)

files %<>%
  str_replace("output/group/data_", "") %>%
  str_replace("[.]rds", "")

names(data) <- files

analyses <- analyse(model, data = data)

saveRDS(analyses, "output/group/analyses.rds")
