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

coef(analysis)

data <- data_set(analysis)

saveRDS(data, "output/group/data_final.rds")

data$fit <- fitted(analysis)$estimate
data$residual <- residuals(analysis)$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = Year, y = residual)) +
  facet_wrap(~Group) +
  geom_point()

models <- model(analysis) %>%
  make_all_models()

analyses <- analyse(models, data = data)

saveRDS(analyses, "output/group/analyses-aic.rds")

coef(analyses[["full"]])

coef <- coef(analyses)
print(coef)

effect <- filter(coef, term %in% c("bPDO", "bArea")) %>%
select(term, estimate, lower, upper) %>%
map_if(is.numeric, exp_minus1) %>%
as.data.frame(stringsAsFactors = FALSE) %>%
as.tbl()

effect$term %<>%
factor(levels = c("bArea", "bPDO"), labels = c("Area", "PDO Index"))

print(ggplot(data = effect, aes(x = term, y = estimate)) +
      geom_pointrange(aes(ymin = lower, ymax = upper)) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_x_discrete("Predictor") +
      scale_y_continuous("Change in Lek Count (%)", labels = percent) +
      expand_limits(y = c(0.33,-0.33)))

write_csv(coef, "output/tables/coef-lek.csv")

data$fit <- exp(predict(analysis, new_data = data, term = "fit")$estimate)

data %<>% complete(Year, Group)

data$GroupABC <- add_ABC(data$Group)

print(
  ggplot(data = data, aes(x = Year, y = Males)) +
    facet_wrap(~GroupABC) +
    geom_line(aes(y = fit)) +
    geom_point() +
    scale_x_continuous("Year") +
    scale_y_continuous("Sage-Grouse (males/lek)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/males-data-group.png", width = 4, height = 4, dpi = dpi)


print(
  ggplot(data = data, aes(x = Year, y = Area)) +
    facet_wrap(~GroupABC) +
    geom_line() +
    scale_x_continuous("Year") +
    scale_y_continuous("Areal Disturbance (%)", labels = percent) +
    expand_limits(y = 0)
)

ggsave("output/plots/area-data-group.png", width = 4, height = 4, dpi = dpi)

