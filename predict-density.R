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

analyses <- readRDS("output/density_analyses.rds")

data <- data_set(analyses)

data %<>%
  lapply(group_by, Year, Group) %>%
  lapply(summarize, Leks = nlevels(Lek), Surveyed = n(), Males = mean(Males)) %>%
  lapply(ungroup) %>%
  bind_rows() %>%
  mutate(Percent = Surveyed / Leks)

data %<>% mutate(Group = factor(Group))

lek_annuals <- lapply(analyses, mcmc_derive_data, c("Lek", "Annual"),
                      parallel = FALSE)

lek_annuals %<>%
  lapply(group_by, Annual) %>%
  lapply(summarise, .fun = mean) %>%
  lapply(ungroup) %>%
  lapply(coef) %>%
  bind_rows(.id = "Group")

lek_annuals %<>% mutate(Year = as.integer(as.character(Annual)))

lek_annuals %<>% complete(Year, Group)

lek_annuals %<>% left_join(data, by = c("Year", "Group"))

lek_annuals %<>% mutate(Group = factor(Group))

lek_annuals %<>% left_join(sampled, by = c("Group", "Year")) %>%
  left_join(data, by = c("Group", "Year"))

print(
  ggplot(data = filter(lek_annuals, Percent >= 0.1, Surveyed >= 5), aes(x = Year, y = estimate)) +
    facet_wrap(~GroupABC) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_x_continuous("Year") +
    scale_y_continuous("Density (males/lek)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/density.png", width = 4, height = 4, dpi = dpi)

saveRDS(lek_annuals, "output/data/density.rds")
