source("header.R")

analysis_ml <- readRDS("output/group/analysis.rds")
analysis <- readRDS("output/group/analysis-bayesian.rds")

data <- data_set(analysis)

with <- derive_data(analysis, new_data = data, term = "kappa")

data %<>% mutate(Area = 0)

without <- derive_data(analysis, new_data = mutate(data, Area = 0), term = "kappa")

loss <- combine_values(with, without, by = c("Group", "Year"), fun = function(x) (x[1] - x[2]) / x[2])

saveRDS(loss, "output/group/loss.rds")

loss %<>% coef()

print(ggplot(data = loss, aes(x = Year, y = estimate)) +
        facet_wrap(~Group) +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dotted") +
        geom_line(aes(y = upper), linetype = "dotted") +
        scale_x_continuous("Year") +
        scale_y_continuous("Loss (%)", labels = percent) +
        expand_limits(y = 0))
