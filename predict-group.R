source("header.R")

analyses <- readRDS("output/group/analyses_final.rds")
dist <- readRDS("output/values/dist.rds")

analyses %<>% sort_by_ic()

data <- data_set(analyses)

data$fit <- fitted(analyses[["full"]])$estimate
data$residual <- residuals(analyses[["full"]])$estimate

ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = data, aes(x = Year, y = residual)) +
  facet_wrap(~Group) +
  geom_point()

coef <- coef(analyses[["full"]])
print(coef)

write_csv(coef, "output/tables/coef-group.csv")

effect <- filter(coef, term %in% c("bPDO", "bArea")) %>%
  select(term, estimate, lower, upper) %>%
  map_if(is.numeric, exp_minus1) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  as.tbl()

effect$term %<>%
  factor(levels = c("bArea", "bPDO"), labels = c("Area", "PDO Index"))

print(ggplot(data = effect, aes(x = term, y = estimate)) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        scale_x_discrete("Predictor") +
        scale_y_continuous("Change in Lek Count (%)", labels = percent) +
        expand_limits(y = c(0.33,-0.33)))

analysis <- readRDS("output/group/analysis_bayesian.rds")

coef <- coef(analysis)
print(coef)


data$fit <- exp(predict(analysis, new_data = data, term = "fit")$estimate)

data %<>% complete(Year, Group)

data$GroupABC <- add_ABC(data$Group)

print(
  ggplot(data = data, aes(x = Year, y = Males)) +
    facet_wrap(~GroupABC) +
    geom_line(aes(y = fit)) +
    geom_point() +
    scale_x_continuous("Year") +
    scale_y_continuous("Sage-Grouse (males/lek)", labels = comma) +
    expand_limits(y = 0)
)

ggsave("output/plots/males-data-group.png", width = 4, height = 4, dpi = dpi)


print(
  ggplot(data = data, aes(x = Year, y = Area)) +
    facet_wrap(~GroupABC) +
    geom_line() +
    scale_x_continuous("Year") +
    scale_y_continuous("Areal Disturbance (%)", labels = percent) +
    expand_limits(y = 0)
)

ggsave("output/plots/area-data-group.png", width = 4, height = 4, dpi = dpi)

