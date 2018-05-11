source("header.R")

analyses <- readRDS("output/density_analyses.rds")

data <- data_set(analyses)

data %<>%
  lapply(group_by, Year, Group) %>%
  lapply(summarize, Leks = nlevels(Lek), Surveyed = n()) %>%
  lapply(ungroup) %>%
  bind_rows() %>%
  mutate(Percent = Surveyed / Leks)

lek_annuals <- lapply(analyses, mcmc_derive_data, c("Lek", "Annual"),
                      parallel = FALSE)

xx <- lek_annuals
lek_annuals <- xx

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

lek_annuals$GroupABC <- add_ABC(lek_annuals$Group)

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
