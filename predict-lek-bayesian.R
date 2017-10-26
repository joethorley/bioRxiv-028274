source("header.R")

analysis <- readRDS("output/lek/analysis-bayesian.rds")

data <- data_set(analysis)

dist <- readRDS("output/values/dist.rds")
warning("need to get dist for real")
dist <- 3200

coef <- coef(analysis)
print(coef)

write_csv(coef, "output/tables/coef-lek.csv")

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

ggsave("output/plots/effect-lek.png", width = 2.5, height = 2.5, dpi = dpi)

saveRDS(nlevels(data$Lek), "output/values/leks-lek.rds")
saveRDS(diff(range(data$Year)) + 1L, "output/values/years-lek.rds")
saveRDS(min(data$Year), "output/values/first-year-lek.rds")
saveRDS(max(data$Year), "output/values/last-year-lek.rds")
saveRDS(sd(data$PDO), "output/values/sd-pdo-lek.rds")
saveRDS(sd(data$Area), "output/values/sd-area-lek.rds")

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
        scale_y_continuous("Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/pdo-lek.png", width = 2.5, height = 2.5, dpi = dpi)

area <- new_data(data, "Area", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data)

print(ggplot(data = area, aes(x = Area, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Oil and Gas Areal Disturbance (%)", labels = percent) +
        scale_y_continuous("Lek Count (%)", labels = percent) +
        expand_limits(y = c(-1,1)))

ggsave("output/plots/area-lek.png", width = 2.5, height = 2.5, dpi = dpi)

annual <- new_data(data, "Annual", ref = ref_data) %>%
  predict(analysis, new_data = ., ref_data = ref_data) %>%
  mutate(Year = as.integer(as.character(Annual)))

print(ggplot(data = annual, aes(x = Year, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_line() +
        geom_point() +
        scale_x_continuous("Year") +
        scale_y_continuous("Males (%)", labels = percent) +
        expand_limits(y = c(-1, 1)))

lek <- new_data(data, "Lek", ref = ref_data) %>%
  predict(analysis, new_data = .)

print(ggplot(data = lek, aes(x = estimate)) +
        geom_histogram(binwidth = 5, color = "white") +
        scale_x_continuous("Males") +
        scale_y_continuous("Leks"))

data %<>%
  group_by(Lek, Year, Group) %>%
  summarise(Males = mean(Males), Area = first(Area)) %>%
  ungroup()

data %<>% complete(nesting(Lek, Group), Year)

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
    scale_y_continuous("Areal Disturbance (%)", labels = percent) +
    expand_limits(y = 0)
)

ggsave("output/plots/area-data-lek.png", width = 4, height = 4, dpi = dpi)

data <- readRDS(paste0("data/analysis/data_", dist, ".rds"))

lag_area <- readRDS("output/values/area_lag_lek.rds")

warning("get real lag area")
lag_area <- 1

lag_pdo <- readRDS("output/values/pdo_lag_lek.rds")

warning("get real lag area")
lag_pdo <- 1

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

data %<>% mutate(Annual = factor(Year, levels = levels(data_set$Annual)),
                 Group = factor(Group, levels = levels(data_set$Group)),
                 Lek = factor(Lek, levels = levels(data_set$Lek)),
                 Males = 1L,
                 Dispersion = factor(data_set$Dispersion, levels = levels(data_set$Dispersion)))

data$PDO <- 0

with <- derive_data(analysis, new_data = data)

with %<>% group_by(Annual, Group) %>%
  summarise() %>%
  ungroup()

without <- derive_data(analysis, new_data = mutate(data, Area = 0))

without %<>% group_by(Annual, Group) %>%
  summarise() %>%
  ungroup()

loss <- combine_values(with, without, by = c("Annual", "Group"), fun = function(x) (x[1] - x[2]) / x[2])

loss %<>% coef()

loss$Year <- loss$Annual %>% as.character() %>% as.integer()

loss$GroupABC <- add_ABC(loss$Group)

print(ggplot(data = loss, aes(x = Year, y = estimate)) +
        facet_wrap(~GroupABC) +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Year") +
        scale_y_continuous("Loss (%)", labels = percent) +
        expand_limits(y = 0))

ggsave("output/plots/loss-lek.png", width = 4, height = 4, dpi = dpi)
