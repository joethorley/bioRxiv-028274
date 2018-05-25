source("header.R")

sampled <- readRDS("output/lek/sampled.rds")

sampled %<>% mutate(ProportionSampled = LeksSurveyed / LeksPopulation,
                    ProportionSize = SizeSurveyed / SizePopulation,
                    ProportionArea = AreaSurveyed / AreaPopulation)

sampled$GroupABC <- add_ABC(sampled$Group)

print(
  ggplot(data = sampled, aes(x = Year, y = ProportionSize)) +
    facet_wrap(~GroupABC) +
    geom_line() +
    geom_point(aes(alpha = ProportionSampled, size = LeksSurveyed)) +
    scale_y_continuous("Lek Size Bias (%)", labels = percent) +
    scale_x_continuous("Year") +
    expand_limits(y = 0)
)

ggsave("output/plots/lek-size-bias.png", width = 8, height = 8, dpi = dpi)

print(
  ggplot(data = sampled, aes(x = Year, y = ProportionArea)) +
    facet_wrap(~GroupABC) +
    geom_line() +
    geom_point(aes(alpha = ProportionSampled, size = LeksSurveyed)) +
    scale_y_continuous("Lek Disturbance Bias (%)", labels = percent) +
    scale_x_continuous("Year") +
    expand_limits(y = 0)
)

ggsave("output/plots/lek-disturbance-bias.png", width = 8, height = 8, dpi = dpi)

sampled %<>%  mutate(Group = as.character(Group))

dist <- readRDS("output/values/dist.rds")

files <- list.files("output/tidy", pattern = str_c("^data_", dist, "_"), full.names = TRUE)

data <- lapply(files, readRDS)

process_data <- function(x) {
  x %<>%
    group_by(Lek, Year, Group) %>%
    summarise(MaxMales = max(Males),
              Males = mean(Males),
              Area = first(Area), PDO = first(PDO)) %>%
    ungroup() %>%
    group_by(Group, Year) %>%
    summarise(
      Leks = sum(!is.na(Males)),
      Males = mean(Males, na.rm = TRUE),
      MaxMales = mean(MaxMales, na.rm = TRUE),
      Area = mean(Area),
      PDO = first(PDO)) %>%
    ungroup() %>%
    filter(!is.na(Males)) %>%
    inner_join(select(sampled, Group, Year, ProportionSampled), by = c("Group", "Year"))

  previous <- select(x, Year, Group, Males, MaxMales) %>%
    mutate(Year = Year + 1L) %>%
    rename(Males1 = Males,
           MaxMales1 = MaxMales)

  x %<>% inner_join(previous, by = c("Group", "Year")) %>%
    mutate(
      Annual = factor(Year),
      Group = factor(Group)
    )
  x
}

data %<>% lapply(process_data)

files %<>% str_replace("/tidy/", "/group/")

x <- map2(data, files, function(x, y) { saveRDS(x, y) })
