source("header.R")

with_lek <- readRDS("output/lek/with.rds")
without_lek <- readRDS("output/lek/without.rds")

# get odds across state

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


#
# loss <- combine_values(with, without, by = c("Annual", "Group"), fun = function(x) (x[1] - x[2]) / x[2])
#
# saveRDS(loss, "output/lek/loss.rds")
#
# loss %<>% coef()
#
# loss$Year <- loss$Annual %>% as.character() %>% as.integer()
#
# loss$GroupABC <- add_ABC(loss$Group)
#
# print(ggplot(data = loss, aes(x = Year, y = estimate)) +
#         facet_wrap(~GroupABC) +
#         geom_line() +
#         geom_line(aes(y = lower), linetype = "dotted") +
#         geom_line(aes(y = upper), linetype = "dotted") +
#         scale_x_continuous("Year") +
#         scale_y_continuous("Loss (%)", labels = percent) +
#         expand_limits(y = 0))
#
# with %<>% group_by(Annual) %>%
#   summarise() %>%
#   ungroup()
#
# without %<>% group_by(Annual) %>%
#   summarise() %>%
#   ungroup()
#
# loss_all <- combine_values(with, without, by = c("Annual"), fun = function(x) (x[1] - x[2]) / x[2])
#
# loss_all %<>% coef()
#
# loss_all %<>% select(Annual, estimate, lower, upper)
#
# saveRDS(loss_all, "output/lek/loss_all.rds")

