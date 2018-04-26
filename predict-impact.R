source("header.R")

with_lek <- readRDS("output/lek/with.rds")
without_lek <- readRDS("output/lek/without.rds")

with_group <- readRDS("output/group/with.rds")
without_group <- readRDS("output/group/without.rds")

loss_lek <- combine_samples(with_lek, without_lek, by = c("Group", "Year"), fun = function(x) (x[1] - x[2]) / x[2])

loss_group <- combine_samples(with_group, without_group, by = c("Group", "Year"), fun = function(x) (x[1] - x[2]) / x[2])

loss_lek %<>% coef()
loss_group %<>% coef()

loss_lek$Group %<>% add_ABC()
loss_group$Group %<>% add_ABC()

print(ggplot(data = loss_group, aes(x = Year, y = estimate)) +
        facet_wrap(~Group) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 1/3) +
        geom_ribbon(data = loss_lek, aes(ymin = lower, ymax = upper), fill = "red", alpha = 1/3) +
        geom_line(color = "blue") +
        geom_line(data = loss_lek, color = "red") +
        scale_x_continuous("Year") +
        scale_y_continuous("Effect on Population Abundance (%)", labels = percent) +
        expand_limits(y = 0))

ggsave("output/plots/impact.png", width = 4, height = 4, dpi = dpi)

with_lek %<>% group_by(Year) %>%
  summarise() %>%
  ungroup()

without_lek %<>% group_by(Year) %>%
  summarise() %>%
  ungroup()

with_group %<>% group_by(Year) %>%
  summarise() %>%
  ungroup()

without_group %<>% group_by(Year) %>%
  summarise() %>%
  ungroup()

loss_lek <- combine_samples(with_lek, without_lek, by = "Year", fun = function(x) (x[1] - x[2]) / x[2])

loss_group <- combine_samples(with_group, without_group, by = "Year", fun = function(x) (x[1] - x[2]) / x[2])

names(loss_group$mcmc) <- "prediction"

geq <- combine_samples(loss_lek, loss_group, by = "Year", function(x) {ifelse(x[1] > x[2], 0, 1)})

geq %<>% coef(estimate = mean) %>%
  filter(Year == 2016)

odds <- geq$estimate / (1- geq$estimate)
odds %<>% round(1)
print(odds)
# get odds for 2016
saveRDS(odds, "output/values/odds.rds")
