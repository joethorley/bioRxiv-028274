source("header.R")

analyses <- readRDS("output/lek/analyses_final.rds")
analysis <- readRDS("output/lek/analysis_bayesian.rds")
dist <- readRDS("output/values/dist.rds")

data <- data_set(analyses)

saveRDS(nlevels(data$Lek), "output/values/leks-lek.rds")
saveRDS(diff(range(data$Year)) + 1L, "output/values/years-lek.rds")
saveRDS(min(data$Year), "output/values/first-year-lek.rds")
saveRDS(max(data$Year), "output/values/last-year-lek.rds")
saveRDS(sd(data$PDO), "output/values/sd-pdo-lek.rds")
saveRDS(sd(data$Area), "output/values/sd-area-lek.rds")

analyses %<>% sort_by_ic()

data$fit <- fitted(analyses[["full"]])$estimate
data$residual <- residuals(analyses[["full"]])$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = fit, y = residual)) +
  geom_point(alpha = 1/5) +
  expand_limits(y = 0)

warning("ensure profiling switched on when in paper model")
coef_full <- coef(analyses[["full"]])
print(coef_full)

write_csv(coef_full, "output/tables/lek-coef-full.csv")

warning("need to profile and MATA on CLs")
coef_mmi <- coef(analyses)
print(coef_mmi)

write_csv(coef_mmi, "output/tables/lek-coef-mmi.csv")

coef_bayesian <- coef(analysis)
print(coef_bayesian)

write_csv(coef_bayesian, "output/tables/lek-coef-bayesian.csv")

effect_full <- get_effects(coef_full)
effect_mmi <- get_effects(coef_mmi)
effect_bayesian <- get_effects(coef_bayesian)

effect_bayesian$Type <- "Bayesian"
effect_full$Type <- "Full"
effect_mmi$Type <- "Averaged"

effect <- bind_rows(effect_bayesian, effect_full, effect_mmi)
effect$Type %<>% factor(levels = c("Averaged", "Full", "Bayesian"))

effect$Term <- str_replace(effect$term, "Area", "Oil and Gas")

print(ggplot(data = effect, aes(x = Type, y = estimate)) +
        facet_wrap(~Term) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_discrete("Statistical Method") +
        scale_y_continuous("Effect on Lek Count (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

ggsave("output/plots/effect-lek.png", width = 4, height = 2.5, dpi = dpi)

ref_data <- new_data(data) %>%
  mutate(Area = 0, PDO = 0)

pdo <- new_data(data, "PDO", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data)

print(ggplot(data = pdo, aes(x = PDO, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("PDO Index") +
        scale_y_continuous("Effect on Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/pdo-lek.png", width = 2.5, height = 2.5, dpi = dpi)

area <- new_data(data, "Area", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data)

print(ggplot(data = area, aes(x = Area, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Oil and Gas (%)", labels = percent) +
        scale_y_continuous("Effect on Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/area-lek.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        scale_x_continuous("Year") +
        scale_y_continuous("Effect on Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/annual-lek.png", width = 2.5, height = 2.5, dpi = dpi)

lek <- new_data(data, "Lek", ref = ref_data) %>%
  predict(analysis, new_data = .)

print(ggplot(data = lek, aes(x = estimate)) +
        geom_histogram(binwidth = 5, color = "white") +
        scale_x_continuous("Lek Size (males)") +
        scale_y_continuous("Leks"))

ggsave("output/plots/leks-lek.png", width = 2.5, height = 2.5, dpi = dpi)

data %<>%
  group_by(Lek, Year, Group) %>%
  summarise(Males = mean(Males), Area = first(Area)) %>%
  ungroup()

data %<>% complete(nesting(Lek, Group), Year)

data$GroupABC <- add_ABC(data$Group)

print(
  ggplot(data = data, aes(x = Year, y = Males)) +
    facet_wrap(~GroupABC) +
    geom_line(aes(group = Lek), alpha = 1/5) +
    scale_x_continuous("Year") +
    scale_y_continuous("Lek Count (males)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/males-data-lek.png", width = 4, height = 4, dpi = dpi)

print(
  ggplot(data = data, aes(x = Year, y = Area)) +
    facet_wrap(~GroupABC) +
    geom_line(aes(group = Lek), alpha = 1/3) +
    scale_x_continuous("Year") +
    scale_y_continuous("Oil and Gas (%)", labels = percent) +
    expand_limits(y = 0)
)

ggsave("output/plots/area-data-lek.png", width = 4, height = 4, dpi = dpi)

data <- readRDS(paste0("data/analysis/data_", dist, ".rds"))

lag_area <- readRDS("output/values/area_lag_lek.rds")
lag_pdo <- readRDS("output/values/pdo_lag_lek.rds")

data %<>% select(Lek, Group, Year, Area, PDO) %>%
  distinct()

area_lagged <- data %>%
  mutate(Year = Year + lag_area) %>%
  select(Lek, Year, Area) %>%
  unique()

pdo_lagged <- data %>%
  mutate(Year = Year + lag_pdo) %>%
  select(Lek, Year, PDO) %>%
  unique()

data %<>%
  select(-Area, -PDO) %>%
  inner_join(area_lagged, by = c("Lek", "Year")) %>%
  inner_join(pdo_lagged, by = c("Lek", "Year")) %>%
  filter(Year %in% first_year:last_year)

data_set <- data_set(analysis) %>%
  new_data()

data %<>% mutate(PDO = 0,
                 Annual = data_set$Annual,
                 Group = factor(Group, levels = levels(data_set$Group)),
                 Lek = factor(Lek, levels = levels(data_set$Lek)),
                 Males = 1L)

with <- derive_data(analysis, new_data = data)

with %<>% group_by(Year, Group) %>%
  summarise() %>%
  ungroup()

without <- derive_data(analysis, new_data = mutate(data, Area = 0))

without %<>% group_by(Year, Group) %>%
  summarise() %>%
  ungroup()

saveRDS(with, "output/lek/with.rds")
saveRDS(without, "output/lek/without.rds")
