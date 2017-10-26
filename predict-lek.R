source("header.R")

saveRDS(dists/1000, "output/values/dists.rds")
saveRDS(min_leks, "output/values/min_leks.rds")

analyses <- readRDS("output/lek/analyses.rds")

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

saveRDS(data, "output/lek/data_final.rds")

data$fit <- fitted(analysis)$estimate
data$residual <- residuals(analysis)$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = fit, y = residual)) +
  geom_point(alpha = 1/5) +
  expand_limits(y = 0)
