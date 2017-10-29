source("header.R")

lek <- readRDS("output/lek/loss.rds")
group <- readRDS("output/group/loss.rds")

group$data %<>% select(Group, Year)
names(group$mcmcr) <- "prediction"

lek$data %<>% rename(Year = Annual) %>%
  mutate(Year = as.integer(as.character(Year)))

geq <- combine_values(lek, group, by = c("Group", "Year"), function(x) {ifelse(x[1] > x[2], 0, 1)})

geq %<>% coef(estimate = mean)

geq$Group %<>% add_ABC()

print(ggplot(data = geq, aes(x = Year, y = estimate)) +
        facet_wrap(~Group) +
        geom_line() +
        scale_x_continuous("Year") +
        scale_y_continuous("Overestimation (%)", labels = percent) +
        expand_limits(y = c(0,1)))

ggsave("output/plots/overestimation.png", width = 4, height = 4, dpi = dpi)

lek %<>% coef()
group %<>% coef()

lek$Group %<>% add_ABC()
group$Group %<>% add_ABC()

print(ggplot(data = group, aes(x = Year, y = estimate)) +
        facet_wrap(~Group) +
        geom_ribbon(data = group, aes(ymin = lower, ymax = upper), fill = "blue", alpha = 1/3) +
        geom_ribbon(data = lek, aes(ymin = lower, ymax = upper), fill = "red", alpha = 1/3) +
        geom_line(color = "blue") +
        geom_line(data = lek, color = "red") +
        scale_x_continuous("Year") +
        scale_y_continuous("Loss (%)", labels = percent) +
        expand_limits(y = 0))

ggsave("output/plots/loss.png", width = 4, height = 4, dpi = dpi)
