source("header.R")

dir.create("output/tidy", recursive = TRUE, showWarnings = FALSE)

pdo <- readRDS("output/clean/pdo.rds")
leks <- readRDS("output/clean/leks.rds")
counts <- readRDS("output/clean/counts.rds")
wells <- readRDS("output/clean/wells.rds")

for (dist in dists) {

  data <- st_buffer(leks, dist) %>%
    st_intersects(wells) %>%
    map(function(x, wells) {slice(wells, x)}, wells = wells) %>%
    map(st_fortify) %>%
    map(accumulate_wells, first_year = first_year - max(lags),
        last_year = last_year) %>%
    set_names(leks$Lek) %>%
    bind_rows(.id = "Lek") %>%
    inner_join(st_fortify(leks), by = "Lek") %>%
    select(-x, -y) %>%
    left_join(counts, by = c("Lek", "Year")) %>%
    inner_join(pdo, by = "Year")

  for (lag_wells in lags) {
    for (lag_pdo in lags) {

      print(str_c("dist: ", dist, " lag_wells: ", lag_wells, " log_pdo: ", lag_pdo))

      wells_lagged <- data %>%
        mutate(Year = Year + lag_wells) %>%
        select(Lek, Year, Wells) %>%
        unique()

      pdo_lagged <- data %>%
        mutate(Year = Year + lag_pdo) %>%
        select(Lek, Year, PDO) %>%
        unique()

      data_lagged <- data %>%
        select(-Wells, -PDO) %>%
        inner_join(wells_lagged, by = c("Lek", "Year")) %>%
        inner_join(pdo_lagged, by = c("Lek", "Year")) %>%
        filter(Year %in% first_year:last_year)

      saveRDS(data_lagged, str_c("output/tidy/data",
                          "_", dist, "_", lag_wells, "_", lag_pdo,".rds"))
    }
  }
}
