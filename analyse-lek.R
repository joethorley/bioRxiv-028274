source("header.R")
source("model-lek.R")

files <- list.files("output/lek", pattern = "^data[_]\\d", full.names = TRUE)

data <- lapply(files, readRDS)

files %<>%
  str_replace("output/lek/data_", "") %>%
  str_replace("[.]rds", "")

names(data) <- files

analyses <- analyse(model, data = data)

saveRDS(analyses, "output/lek/analyses_lags.rds")

glance <- map(analyses, glance) %>%
  bind_rows(.id = "code")

glance %<>% mutate(Distance = str_extract(code, "^[^_]+"),
                   LagArea = str_replace(code, "(^[^_]+_)([^_]+)(_[^_]+$)", "\\2"),
                   LagPDO = str_extract(code, "[^_]+$"))

glance %<>% arrange(IC)

glance %<>% mutate(DeltaIC = IC - min(IC),
                   ICWt = exp(-0.5 * DeltaIC))

glance$ICWt %<>% divide_by(., sum(.))

distance <- glance %>%
  group_by(Distance) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(distance)

write_csv(distance, "output/tables/distance-lek.csv")

pdo <- glance %>%
  group_by(LagPDO) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(pdo)

write_csv(pdo, "output/tables/pdo-lek.csv")

area <- glance %>%
  group_by(LagArea) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(area)

write_csv(area, "output/tables/area-lek.csv")

dist <- as.numeric(distance$Distance[1])

saveRDS(dist, "output/values/dist.rds")
saveRDS(as.integer(area$LagArea[1]), "output/values/area_lag_lek.rds")
saveRDS(as.integer(pdo$LagPDO[1]), "output/values/pdo_lag_lek.rds")

analysis <- analyses[[str_c(distance$Distance[1], area$LagArea[1], pdo$LagPDO[1], sep = "_")]]

data <- data_set(analysis)

models <- model(analysis) %>%
  make_all_models()

analyses <- analyse(models, data = data)

saveRDS(analyses, "output/lek/analyses_final.rds")

analysis <- analyse(model_bayesian, data = data)

saveRDS(analysis, "output/lek/analysis_bayesian.rds")
