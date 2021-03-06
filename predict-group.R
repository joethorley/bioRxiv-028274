source("header.R")

analyses <- readRDS("output/group/analyses_final.rds")
analysis <- readRDS("output/group/analysis_bayesian.rds")
dist <- readRDS("output/values/dist.rds")

data <- data_set(analyses)

saveRDS(sd(data$PDO), "output/values/sd-pdo-group.rds")
saveRDS(sd(data$Area), "output/values/sd-area-group.rds")

analyses %<>% sort_by_ic()

data$fit <- fitted(analyses[["full"]])$estimate
data$residual <- residuals(analyses[["full"]])$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = Year, y = residual)) +
  facet_wrap(~Group) +
  geom_point()

R2c <- R2(analyses[["full"]], response = "Males")
R2m <- R2(analyses[["full"]], response = "Males", marginal = TRUE)

saveRDS(R2c, "output/values/R2c_group.rds")
saveRDS(R2m, "output/values/R2m_group.rds")

coef_full <- coef(analyses[["full"]])
print(coef_full)

print(readRDS("output/values/area_lag_group.rds"))
print(readRDS("output/values/pdo_lag_group.rds"))

print(glance(analyses[["full"]]))

write_csv(coef_full, "output/tables/group-coef-full.csv")

coef_mmi <- coef(analyses)
print(coef_mmi)

write_csv(coef_mmi, "output/tables/group-coef-mmi.csv")

coef_bayesian <- coef(analysis)
print(coef_bayesian)

write_csv(coef_bayesian, "output/tables/group-coef-bayesian.csv")

effect_full <- get_effects(coef_full)
effect_mmi <- get_effects(coef_mmi)
effect_bayesian <- get_effects(coef_bayesian)

effect_full$Type <- "Full"
effect_mmi$Type <- "Averaged"
effect_bayesian$Type <- "Bayesian"

effect <- bind_rows(effect_bayesian, effect_full, effect_mmi)
effect$Type %<>% factor(levels = c("Averaged", "Full", "Bayesian"))

effect$Term <- str_replace(effect$term, "Area", "Oil and Gas (A)")
effect$Term %<>% str_replace("PDO Index", "PDO Index (B)")

print(ggplot(data = effect, aes(x = Type, y = estimate)) +
        facet_wrap(~Term) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_discrete("Statistical Method") +
        scale_y_continuous("Effect on Subsequent Density (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

ggsave("output/plots/effect-group.png", width = 4, height = 2.5, dpi = dpi)

data$fit <- predict(analyses[["full"]], new_data = data, term = "fit")$estimate

data %<>% complete(Year, Group)

data$GroupABC <- add_ABC(data$Group)

print(
  ggplot(data = data, aes(x = Year, y = Males)) +
    facet_wrap(~GroupABC) +
    geom_line(aes(y = fit)) +
    geom_point(size = 1) +
    scale_x_continuous("Year") +
    scale_y_continuous("Density (males/lek)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/males-data-group.png", width = 4, height = 4, dpi = dpi)

print(
  ggplot(data = data, aes(x = Year, y = Area)) +
    facet_wrap(~GroupABC) +
    geom_line() +
    scale_x_continuous("Year") +
    scale_y_continuous("Oil and Gas (%)", labels = percent) +
    expand_limits(y = 0)
)

ggsave("output/plots/area-data-group.png", width = 4, height = 4, dpi = dpi)

options(mb.parallel = FALSE)

ref_data <- new_data(data) %>%
  mutate(Area = 0, PDO = 0)

males <- new_data(data, "Males1", ref = ref_data) %>%
  predict(analysis, new_data = .)

males %<>% mutate(estimate = (estimate - Males1) / Males1,
                  lower = (lower - Males1) / Males1,
                  upper = (upper - Males1) / Males1)

print(ggplot(data = males, aes(x = Males1, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Density (males/lek)") +
        scale_y_continuous("Effect on Subsequent Density (%)", labels = percent))

ggsave("output/plots/density-group.png", width = 2.5, height = 2.5, dpi = dpi)

pdo <- new_data(data, "PDO", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = pdo, aes(x = PDO, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("PDO Index") +
        scale_y_continuous("Effect on Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/pdo-group.png", width = 2.5, height = 2.5, dpi = dpi)

area <- new_data(data, "Area", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = area, aes(x = Area, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Oil and Gas (%)", labels = percent) +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/area-group.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        scale_x_continuous("Year") +
        scale_y_continuous("Effect on Subsequent Density (%)", labels = percent) +
      expand_limits(y = c(-0.5,0.5)))

ggsave("output/plots/annual-group.png", width = 2.5, height = 2.5, dpi = dpi)

group <- new_data(data, "Group", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa")

print(ggplot(data = group, aes(x = Group, y = estimate)) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        scale_x_discrete("Working Group") +
        scale_y_continuous("Carrying Capacity (males/lek)") +
        expand_limits(y = 0) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))

ggsave("output/plots/group-group.png", width = 2.5, height = 3, dpi = dpi)

data <- data_set(analysis)
data_set <- new_data(data)

data %<>% mutate(PDO = 0,
                 Annual = data_set$Annual)

with <- mcmc_derive_data(analysis, new_data = data, term = "kappa")
without <- mcmc_derive_data(analysis, new_data = mutate(data, Area = 0), term = "kappa")

saveRDS(with, "output/group/with.rds")
saveRDS(without, "output/group/without.rds")
