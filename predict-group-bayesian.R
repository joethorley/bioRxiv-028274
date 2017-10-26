source("header.R")

analysis <- readRDS("output/group/analysis-bayesian.rds")

data <- data_set(analysis)

dist <- readRDS("output/values/dist.rds")
warning("need to get dist for real")
dist <- 3200

coef <- coef(analysis)
print(coef)

write_csv(coef, "output/tables/coef-group.csv")

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
        scale_y_continuous("Change in Population Density (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

ggsave("output/plots/effect-group.png", width = 2.5, height = 2.5, dpi = dpi)

saveRDS(sd(data$PDO), "output/values/sd-pdo-group.rds")
saveRDS(sd(data$Area), "output/values/sd-area-group.rds")

ref_data <- new_data(data) %>%
  mutate(Area = 0, PDO = 0)

males <- new_data(data, "Males", ref = ref_data) %>%
  predict(analysis, new_data = .)

print(ggplot(data = males, aes(x = Males, y = exp(estimate))) +
        geom_line() +
        geom_line(aes(y = exp(lower)), linetype = "dotted") +
        geom_line(aes(y = exp(upper)), linetype = "dotted") +
        geom_hline(yintercept = 1, linetype = "dotted") +
        scale_x_continuous("Sage-Grouse (males/lek)") +
        scale_y_continuous("Annual Population Change (%)", labels = percent))

pdo <- new_data(data, "PDO", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = pdo, aes(x = PDO, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("PDO Index") +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/pdo-group.png", width = 2.5, height = 2.5, dpi = dpi)

area <- new_data(data, "Area", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = area, aes(x = Area, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Areal Disturbance (%)", labels = percent) +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/area-group.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa", ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_point() +
        scale_x_continuous("Year") +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 2.5)))

group <- new_data(data, "Group", ref = ref_data) %>%
  predict(analysis, new_data = ., term = "kappa")

print(ggplot(data = group, aes(x = Group, y = estimate)) +
        geom_col() +
        scale_x_discrete("Group") +
        scale_y_continuous("Carrying Capacity (males/lek)") +
        expand_limits(y = 0))

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

data <- data_set(analysis)

data_set <- new_data(data)

data %<>% mutate(PDO = 0,
                 Annual = data_set$Annual)

with <- derive_data(analysis, new_data = data, term = "kappa")
without <- derive_data(analysis, new_data = mutate(data, Area = 0), term = "kappa")

loss <- combine_values(with, without, by = c("Group", "Year"), fun = function(x) (x[1] - x[2]) / x[2])

loss %<>% coef()

loss$GroupABC <- add_ABC(loss$Group)

print(ggplot(data = loss, aes(x = Year, y = estimate)) +
        facet_wrap(~GroupABC) +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Year") +
        scale_y_continuous("Loss (%)", labels = percent) +
        expand_limits(y = 0))

ggsave("output/plots/loss-group.png", width = 4, height = 4, dpi = dpi)
