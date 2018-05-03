source("header.R")

analyses <- readRDS("output/density_analyses.rds")

data <- data_set(analyses)

lek_annuals <- lapply(analyses[1:2], mcmc_derive_data, c("Lek", "Annual"))

xx <- lek_annuals
lek_annuals <- xx

lek_annuals %<>%
  lapply(group_by, Annual) %>%
  lapply(summarise, .fun = mean) %>%
  lapply(ungroup) %>%
  lapply(coef) %>%
  bind_rows(.id = "Group")

lek_annuals %<>% mutate(Group = factor(Group),
                        Year = as.integer(as.character(Annual)))

lek_annuals %<>% complete(Year, Group)

lek_annuals$GroupABC <- add_ABC(lek_annuals$Group)

print(
  ggplot(data = lek_annuals, aes(x = Year, y = estimate)) +
    facet_wrap(~GroupABC) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_x_continuous("Year") +
    scale_y_continuous("Density (males/lek)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/density.png", width = 4, height = 4, dpi = dpi)
