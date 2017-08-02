source("header.R")

dir.create("output/tidy", recursive = TRUE, showWarnings = FALSE)

for (dist in dists) {

  data <- readRDS(str_c("data/analysis/data_", dist, ".rds"))

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
