source("header.R")

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)

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
    inner_join(pdo, by = "Year") %>%
    select(Lek, Group, Year, Males, Wells, PDO, Dayte)
    # select required to ensure locational information not made publicly available
  print(data)

  saveRDS(data, str_c("data/analysis/data_", dist, ".rds"))
}
