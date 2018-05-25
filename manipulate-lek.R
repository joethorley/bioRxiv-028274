source("header.R")

files <- list.files("output/tidy", full.names = TRUE)

data <- lapply(files, readRDS)

process_data <- function(x) {
  x %<>%
    group_by(Lek, Year, Group) %>%
    summarise(MaxMales = as.integer(max(Males)),
              Males = as.integer(round(mean(Males))),
              Area = first(Area),
              PDO = first(PDO),
              Counts = n()) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(
      Annual = factor(Year, levels = min(Year):max(Year)),
      Group = factor(Group),
      Lek = factor(Lek),
    ) %>%
    as.tbl()
  x
}

data %<>% lapply(process_data)

files %<>% str_replace("/tidy/", "/lek/")

x <- map2(data, files, function(x, y) { saveRDS(x, y) })
