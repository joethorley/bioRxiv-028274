source("header.R")

dist <- readRDS("output/values/dist.rds")
lag_area <- readRDS("output/values/area_lag_lek.rds")
lag_pdo <- readRDS("output/values/pdo_lag_lek.rds")

pdo <- readRDS("output/clean/pdo.rds")
leks <- readRDS("output/clean/leks.rds")
wells <- readRDS("output/clean/wells.rds")

data <- readRDS(paste0("data/analysis/data_", dist, ".rds"))

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

analysis <- readRDS("output/lek/analysis-bayesian.rds")

data_set <- data_set(analysis) %>%
  new_data()

data %<>% mutate(Annual = factor(Year, levels = levels(data_set$Annual)),
                 Group = factor(Group, levels = levels(data_set$Group)),
                 Lek = factor(Lek, levels = levels(data_set$Lek)),
                 Males = 1L,
                 Dispersion = data_set$Dispersion)

with <- derive_data(analysis, new_data = data)

with %<>% group_by(Annual, Group) %>%
  summarise() %>%
  ungroup()

without <- derive_data(analysis, new_data = mutate(data, Area = 0))

without %<>% group_by(Annual, Group) %>%
  summarise() %>%
  ungroup()

loss <- combine_values(with, without, by = c("Annual", "Group"), fun = function(x) (x[1] - x[2]) / x[2])

saveRDS(loss, "output/lek/loss.rds")

loss$Year <- loss$Annual %>% as.character() %>% as.integer()

loss %<>% coef()

print(ggplot(data = loss, aes(x = Year, y = estimate)) +
        facet_wrap(~Group) +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Year") +
        scale_y_continuous("Loss (%)", labels = percent) +
        expand_limits(y = 0))
