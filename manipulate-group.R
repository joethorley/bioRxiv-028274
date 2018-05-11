source("header.R")

dist <- readRDS("output/values/dist.rds")

files <- list.files("output/tidy", pattern = str_c("^data_", dist, "_"), full.names = TRUE)

data <- lapply(files, readRDS)


process_data <- function(x) {
  density <- readRDS("output/data/density.rds")

  density %<>% filter(Percent >= 0.1, Surveyed >= 5) %>%
    select(Year, Group, Males = estimate, SD = sd, Leks, Surveyed)

  x %<>%
    group_by(Lek, Year, Group) %>%
    summarise(Males = mean(Males), Area = first(Area), PDO = first(PDO)) %>%
    ungroup() %>%
    group_by(Group, Year) %>%
    summarise(
      Area = mean(Area),
      PDO = first(PDO)) %>%
    ungroup() %>%
    inner_join(density, by = c("Year", "Group")) %>%
    ddply("Group", trim_males, min_years = min_years, last_year = last_year) %>%
    mutate(
      Annual = factor(Year, levels = min(Year):max(Year)),
      Group = factor(Group)
    )
  x
}

data %<>% lapply(process_data)

files %<>% str_replace("/tidy/", "/group/")

x <- map2(data, files, function(x, y) { saveRDS(x, y) })
