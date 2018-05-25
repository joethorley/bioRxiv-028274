source("header.R")
source("model-group.R")

analyses <- readRDS("output/group/analyses_final.rds")

data <- data_set(analyses[["full"]])

data2 <- filter(data, Year >= 1997) %>%
  mutate(Annual = droplevels(Annual))

data3 <- mutate(data,
                Males = MaxMales,
                Males1 = MaxMales1)

data4 <- mutate(data2,
                Males = MaxMales,
                Males1 = MaxMales1)

data <- list("1985 Mean" = data,
             "1997 Mean" = data2,
             "1985 Max" = data3,
             "1997 Max" = data4)

model <- model(analyses[["full"]])

analyses <- analyse(model, data = data)

coefs <- lapply(analyses, coef)

effects <- lapply(coefs, get_effects)

effects %<>% bind_rows(.id = "Analysis")

effects %<>% mutate(Year = word(Analysis, 1),
                    Type = word(Analysis, 2))

effects$Term <- str_replace(effects$term, "Area", "Oil and Gas")
effects$Term %<>% str_replace("PDO Index", "PDO Index")

effects$Year %<>% paste("-", last_year)

effects$Year[str_detect(effects$Year, "1985")] %<>% paste("(A)")
effects$Year[str_detect(effects$Year, "1997")] %<>% paste("(B)")

ggplot(data = effects, aes(x = Type, y = estimate)) +
  facet_grid(Term ~ Year) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete("Statistic") +
  scale_y_continuous("Effect on Subsequent Density (%)", labels = percent) +
  expand_limits(y = c(0.33,-0.33))

ggsave("output/plots/sensitivity-group.png", width = 4, height = 4, dpi = dpi)
