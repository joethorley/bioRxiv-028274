source("header.R")

analyses <- readRDS("output/group/analyses.rds")

glance <- map(analyses, glance) %>%
  bind_rows(.id = "code")

glance %<>% mutate(Distance = str_extract(code, "^[^_]+"),
                   LagArea = str_replace(code, "(^[^_]+_)([^_]+)(_[^_]+$)", "\\2"),
                   LagPDO = str_extract(code, "[^_]+$"))

glance %<>% arrange(IC)

glance %<>% mutate(DeltaIC = IC - min(IC),
                   ICWt = exp(-0.5 * DeltaIC))

glance$ICWt %<>% divide_by(., sum(.))

dist <- glance$Distance %>%
  first() %>%
  as.numeric()

pdo <- glance %>%
  group_by(LagPDO) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(pdo)

write_csv(pdo, "output/tables/pdo-group.csv")

area <- glance %>%
  group_by(LagArea) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(area)

write_csv(area, "output/tables/area-group.csv")

saveRDS(as.integer(area$LagArea[1]), "output/values/area_lag_group.rds")
saveRDS(as.integer(pdo$LagPDO[1]), "output/values/pdo_lag_group.rds")

analysis <- analyses[[str_c(dist, area$LagArea[1], pdo$LagPDO[1], sep = "_")]]

data <- data_set(analysis)

saveRDS(data, "output/group/data_final.rds")

data$fit <- fitted(analysis)$estimate
data$residual <- residuals(analysis)$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = Year, y = residual)) +
  facet_wrap(~Group) +
  geom_point()
