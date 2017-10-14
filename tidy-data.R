source("header.R")

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)

pdo <- readRDS("output/clean/pdo.rds")
leks <- readRDS("output/clean/leks.rds")
counts <- readRDS("output/clean/counts.rds")
wells <- readRDS("output/clean/wells.rds")

# buffer each well by 60 m
wells %<>% st_buffer(set_units(60, "m"))

# produce list of sf objects by year
wells_years <- list()
years <- min(wells$Year):max(wells$Year)
for (i in seq_along(years)) {
  wells_years[[i]] <- filter(wells, Year <= years[i])
}
names(wells_years) <- years

for (dist in dists) {

      print(str_c("dist: ", dist))
  data <- st_buffer(leks, set_units(dist, "m")) %>%
    lapply(wells_years, st_intersection_switch, .) %>%
    lapply(function(y) aggregate(y, by = list(Lek = y$Lek), FUN = identity)) %>%
    lapply(select, Lek) %>%
    lapply(function(x) tibble(Lek = x$Lek, Area = st_area(x))) %>%
    bind_rows(.id = "Year") %>%
    mutate(Area = Area / (pi * dist^2)) %>%
    right_join(st_fortify(leks), by = "Lek") %>%
    select(-x, -y) %>%
    replace_na(replace = list(Year = min(wells$Year), Area = 0)) %>%
    complete(Year, nesting(Lek, Group), fill = list(Area = 0)) %>%
    mutate(Year = as.integer(Year)) %>%
    left_join(counts, by = c("Lek", "Year")) %>%
    inner_join(pdo, by = "Year") %>%
    select(Lek, Group, Year, Males, Area, PDO, Dayte)
  # select required to ensure locational information not made publicly available

  saveRDS(data, str_c("data/analysis/data_", dist, ".rds"))
}
