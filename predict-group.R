source("header.R")

analyses <- readRDS("output/group/analyses.rds")

glance <- map(analyses, glance) %>%
  bind_rows(.id = "code")

glance %<>% mutate(Distance = str_extract(code, "^[^_]+"),
                   LagWells = str_replace(code, "(^[^_]+_)([^_]+)(_[^_]+$)", "\\2"),
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

wells <- glance %>%
  group_by(LagWells) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(wells)

write_csv(wells, "output/tables/wells-group.csv")

saveRDS(as.integer(wells$LagWells[1]), "output/values/wells_lag_group.rds")
saveRDS(as.integer(pdo$LagPDO[1]), "output/values/pdo_lag_group.rds")

analysis <- analyses[[str_c(dist, wells$LagWells[1], pdo$LagPDO[1], sep = "_")]]

data <- data_set(analysis)

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

coef <- coef(analyses)
print(coef)

write_csv(coef, "output/tables/coef-group.csv")

effect <- filter(coef, term %in% c("bPDO", "bWells")) %>%
  select(term, estimate, lower, upper) %>%
  map_if(is.numeric, exp_minus1) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  as.tbl()

effect$term %<>%
  factor(levels = c("bWells", "bPDO"), labels = c("Well Pads", "PDO Index"))

print(ggplot(data = effect, aes(x = term, y = estimate)) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_discrete("Predictor") +
        scale_y_continuous("Change in Population Density (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

ggsave("output/plots/effect-group.png", width = 2.5, height = 2.5, dpi = dpi)

saveRDS(sd(data$PDO), "output/values/sd-pdo-group.rds")
saveRDS(sd(data$Wells/dist2area(dist)), "output/values/sd-wells-group.rds")

ref_data <- new_data(data) %>%
  mutate(Wells = 0, PDO = 0)

males <- new_data(data, "Males", ref = ref_data) %>%
  predict(analyses, new_data = .)

print(ggplot(data = males, aes(x = Males, y = exp(estimate))) +
        geom_line() +
        geom_hline(yintercept = 1, linetype = "dotted") +
        scale_x_continuous("Sage-Grouse (males/lek)") +
        scale_y_continuous("Annual Population Change (%)", labels = percent))

pdo <- new_data(data, "PDO", ref = ref_data) %>%
  predict(analyses, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = pdo, aes(x = PDO, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        scale_x_continuous("PDO Index") +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/pdo-group.png", width = 2.5, height = 2.5, dpi = dpi)

wells <- new_data(data, "Wells", ref = ref_data) %>%
  predict(analyses, new_data = ., term = "kappa", ref_data = ref_data)

print(ggplot(data = wells, aes(x = Wells/dist2area(dist), y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        scale_x_continuous("Oil and Gas (well pads/km2)") +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

ggsave("output/plots/wells-group.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analyses, new_data = ., term = "kappa", ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_point() +
        scale_x_continuous("Year") +
        scale_y_continuous("Carrying Capacity (%)", labels = percent) +
        expand_limits(y = c(-1, 2.5)))

group <- new_data(data, "Group", ref = ref_data) %>%
  predict(analyses, new_data = ., term = "kappa")

print(ggplot(data = group, aes(x = Group, y = estimate)) +
        geom_col() +
        scale_x_discrete("Group") +
        scale_y_continuous("Carrying Capacity (males/lek)") +
        expand_limits(y = 0))

data$fit <- exp(predict(analyses, new_data = data, term = "fit")$estimate)

data %<>% complete(Year, Group)

add_ABC <- function(x) {
  if (!length(x)) return(x)

  letters <- letters[1:nlevels(x)] %>%
    toupper() %>%
    paste0("(", ., ")")
  levels(x) %<>% paste(letters, .)
  x
}

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
  ggplot(data = data, aes(x = Year, y = Wells/dist2area(dist))) +
    facet_wrap(~GroupABC) +
    geom_line() +
    scale_x_continuous("Year") +
    scale_y_continuous("Oil and Gas (well pads/km2)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/wells-data-group.png", width = 4, height = 4, dpi = dpi)