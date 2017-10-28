source("header.R")

lek <- readRDS("output/lek/loss.rds")
group <- readRDS("output/group/loss.rds")

group$data %<>% select(Group, Year)
names(group$mcmcr) <- "prediction"

lek$data %<>% rename(Year = Annual) %>%
  mutate(Year = as.integer(as.character(Year)))

geq <- combine_values(lek, group, by = c("Group", "Year"), function(x) {ifelse(x[1] > x[2], 0, 1)})

geq %<>% coef(estimate = mean)

geq$GroupABC <- add_ABC(geq$Group)

print(ggplot(data = geq, aes(x = Year, y = estimate)) +
        facet_wrap(~GroupABC) +
        geom_line() +
        scale_x_continuous("Year") +
        scale_y_continuous("Overestimation (%)", labels = percent) +
        expand_limits(y = c(0,1)))

ggsave("output/plots/overestimation.png", width = 4, height = 4, dpi = dpi)
