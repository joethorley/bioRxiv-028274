source("header.R")

# areal distance not important as just use lek counts
data <- readRDS("data/analysis/data_800.rds")

process_data <- function(x) {
  x %<>%
    group_by(Lek, Year, Group) %>%
    summarise(Males = as.integer(round(mean(Males)))) %>%
    ungroup() %>%
    na.omit() %>%
    mutate(
      Annual = factor(Year),
      Group = factor(Group),
      Lek = factor(Lek)
    ) %>%
    as.tbl()
  x
}

data %<>% process_data()

data %<>% split(data$Group)

process_group_data <- function(x) {
  x$Group %<>% as.character()
  x$Lek %<>% droplevels()
  x$Annual %<>% droplevels()
  x
}

data %<>% lapply(process_group_data)

names(data) %<>% paste0("output/density/", ., ".rds")

x <- map2(data, names(data), function(x, y) { saveRDS(x, y) })
