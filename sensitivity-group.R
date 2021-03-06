source("header.R")
source("model-group.R")

analyses <- readRDS("output/group/analyses_final.rds")

data <- data_set(analyses[["full"]])

data97 <- filter(data, Year >= 1997) %>%
  mutate(Annual = droplevels(Annual))

data05 <- filter(data, Year >= 2005) %>%
  mutate(Annual = droplevels(Annual))

data2 <- mutate(data,
                Males = MaxMales,
                Males1 = MaxMales1)

data972 <- mutate(data97,
                Males = MaxMales,
                Males1 = MaxMales1)

data052 <- mutate(data05,
                Males = MaxMales,
                Males1 = MaxMales1)

data <- list("1985 Mean" = data,
             "1997 Mean" = data97,
             "2005 Mean" = data05,
             "1985 Max" = data2,
             "1997 Max" = data972,
             "2005 Max" = data052)

analyses <- analyse(model, data = data)

sds <- lapply(data, function(x) {
  as.data.frame(lapply(select(x, Males, MaxMales, PDO), sd))})

sds %<>% bind_rows(.id = "Analysis")

sds %<>% gather("term", "sd", -Analysis)

sds %<>% filter(term == "PDO" | str_detect(Analysis, "Max") == (term == "MaxMales"))

sds$term %<>% str_replace("PDO", "bPDO") %>%
  str_replace("MaxMales|Males", "bArea")

sds %<>% mutate(Year = word(Analysis, 1),
                Type = word(Analysis, 2),
                Type = factor(Type, levels = c("Mean", "Max")),
                Analysis = NULL) %>%
  arrange(Year, Type)

sds %<>% ddply(.(term), function(x) mutate(x, sd = sd / first(sd)))

coefs <- lapply(analyses, coef) %>%
  bind_rows(.id = "Analysis")

coefs %<>% mutate(Year = word(Analysis, 1),
                  Type = word(Analysis, 2),
                  Type = factor(Type, levels = c("Mean", "Max")),
                  Analysis = NULL,
                  sd = NULL) %>%
  filter(term %in% c("bPDO", "bArea"))

coefs %<>% inner_join(sds, by = c("term", "Year", "Type"))

coefs %<>% mutate(estimate = estimate / sd,
                  lower = lower / sd,
                  upper = upper / sd)

effects <- ddply(coefs, .(Year, Type), get_effects)

effects$term %<>%
  str_replace("Area", "Oil and Gas") %>%
  str_replace("PDO Index", "PDO Index")


effects$Year %<>% paste("-", last_year)

effects$Year[str_detect(effects$Year, "1985")] %<>% paste("(A)")
effects$Year[str_detect(effects$Year, "1997")] %<>% paste("(B)")
effects$Year[str_detect(effects$Year, "2005")] %<>% paste("(C)")

ggplot(data = effects, aes(x = Type, y = estimate)) +
  facet_grid(term ~ Year, scales = "free_y") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete("Statistic") +
  scale_y_continuous("Effect on Subsequent Density (%)", labels = percent) +
  expand_limits(y = 0)

ggsave("output/plots/sensitivity-group.png", width = 4, height = 4, dpi = dpi)
