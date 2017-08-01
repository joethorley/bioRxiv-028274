source("header.R")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

analyses <- readRDS("output/lek/analyses.rds")
leks <- readRDS("output/clean/leks.rds")

glance <- map(analyses, glance) %>%
  bind_rows(.id = "code")

glance %<>% mutate(Distance = str_extract(code, "^[^_]+"),
                   LagWells = str_replace(code, "(^[^_]+_)([^_]+)(_[^_]+$)", "\\2"),
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

wells <- glance %>%
  group_by(LagWells) %>%
  summarise(nmodels = n(),
            proportion = n()/nrow(glance),
            ICWt = sum(ICWt)) %>%
  ungroup() %>%
  arrange(-ICWt)

print(wells)

write_csv(wells, "output/tables/wells-lek.csv")

dist <- as.numeric(distance$Distance[1])

saveRDS(dist, "output/values/dist.rds")
saveRDS(as.integer(wells$LagWells[1]), "output/values/wells_lag_lek.rds")
saveRDS(as.integer(pdo$LagPDO[1]), "output/values/pdo_lag_lek.rds")

analysis <- analyses[[str_c(distance$Distance[1], wells$LagWells[1], pdo$LagPDO[1], sep = "_")]]

data <- data_set(analysis)

data$fit <- fitted(analysis)$estimate
data$residual <- residuals(analysis)$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = fit, y = residual)) +
  geom_point(alpha = 1/5) +
  expand_limits(y = 0)

models <- model(analysis) %>%
  make_all_models()

analyses <- analyse(models, data = data)

coef <- coef(analyses)
print(coef)

write_csv(coef, "output/tables/coef-lek.csv")

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
        scale_y_continuous("Change in Lek Count (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

ggsave("output/plots/effect-lek.png", width = 2.5, height = 2.5, dpi = dpi)

saveRDS(nlevels(data$Lek), "output/values/leks-lek.rds")
saveRDS(diff(range(data$Year)) + 1L, "output/values/years-lek.rds")
saveRDS(min(data$Year), "output/values/first-year-lek.rds")
saveRDS(max(data$Year), "output/values/last-year-lek.rds")
saveRDS(sd(data$PDO), "output/values/sd-pdo-lek.rds")
saveRDS(sd(data$Wells/dist2area(dist)), "output/values/sd-wells-lek.rds")

ref_data <- new_data(data) %>%
  mutate(Wells = 0, PDO = 0)

pdo <- new_data(data, "PDO", ref = ref_data) %>%
  predict(analyses, new_data = ., ref_data = ref_data)

print(ggplot(data = pdo, aes(x = PDO, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        scale_x_continuous("PDO Index") +
        scale_y_continuous("Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/pdo-lek.png", width = 2.5, height = 2.5, dpi = dpi)

wells <- new_data(data, "Wells", ref = ref_data) %>%
  predict(analyses, new_data = ., ref_data = ref_data)

print(ggplot(data = wells, aes(x = Wells/dist2area(dist), y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        scale_x_continuous("Oil and Gas (well pads/km2)") +
        scale_y_continuous("Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/wells-lek.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analyses, new_data = ., ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_point() +
        scale_x_continuous("Year") +
        scale_y_continuous("Males (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

lek <- new_data(data, "Lek", ref = ref_data) %>%
  predict(analyses, new_data = .)

print(ggplot(data = lek, aes(x = estimate)) +
        geom_histogram(binwidth = 5, color = "white") +
        scale_x_continuous("Males") +
        scale_y_continuous("Leks"))

data %<>%
  group_by(Lek, Year, Group) %>%
  summarise(Males = mean(Males), Wells = first(Wells)) %>%
  ungroup()

data %<>% complete(nesting(Lek, Group), Year)

print(
  ggplot(data = data, aes(x = Year, y = Males)) +
    facet_wrap(~Group) +
    geom_line(aes(group = Lek), alpha = 1/5) +
    scale_x_continuous("Year") +
    scale_y_continuous("Lek Count (males)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/males-data-lek.png", width = 4, height = 4, dpi = dpi)

print(
  ggplot(data = data, aes(x = Year, y = Wells/dist2area(dist))) +
    facet_wrap(~Group) +
    geom_line(aes(group = Lek), alpha = 1/3) +
    scale_x_continuous("Year") +
    scale_y_continuous("Oil and Gas (well pads/km2)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/wells-data-lek.png", width = 4, height = 4, dpi = dpi)
