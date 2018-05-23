source("header.R")
source("model-group2.R")

analyses <- readRDS("output/group/analyses_final.rds")
analysis <- analyses[["full"]]

data <- data_set(analysis)

generate_data <- function(analysis, bArea = NULL) {

  data <- data_set(analysis, modify = TRUE)
  estimates <- estimates(analysis)

  if(!is.null(bArea))
    data$bArea <- bArea

  males <- with(c(data, estimates), {
    sAnnual <- exp(log_sAnnual)
    sGroup <- exp(log_sGroup)
    sInitial <- exp(log_sInitial)
    sProcess <- exp(log_sProcess)
    sObservation <- exp(log_sObservation)

    bAnnual <- rnorm(nAnnual, 0, sAnnual)
    bInitial <- rnorm(nGroup, bInitialIntercept, sInitial)
    bGroup <- rnorm(nGroup, 0, sGroup)
    bProcess <- rnorm(nAnnual * nGroup, 0, sProcess) %>% matrix(nrow = nGroup)
    bObservation <- rnorm(nAnnual * nGroup, 0, sObservation) %>% matrix(nrow = nGroup)

    is.na(Males) <- TRUE

    for(i in 1:nGroup) {
      Males[i, FirstAnnual[i]] <- bIntercept + (bDensity + 1 + bGroup[i]) * bInitial[i] + bArea * Area[i,FirstAnnual[i]] + bPDO * PDO[i,FirstAnnual[i]] + bAnnual[FirstAnnual[i]]

      for(j in (FirstAnnual[i]+1):nAnnual) {
        Males[i,j] <- bIntercept + (bDensity + 1 + bGroup[i]) * Males[i,j-1] + bArea * Area[i,j] + bPDO * PDO[i,j] + bAnnual[j] + bProcess[i,j]
      }
    }
    exp(Males)
  })

  males <- as.data.frame(males) %>%
    mutate(., Group = factor(rownames(.))) %>%
    gather("Year", "Males", -Group) %>%
    mutate(Year = as.integer(Year)) %>%
    na.omit()

  data <- data_set(analysis) %>%
    select(-Males) %>%
    inner_join(males, by = c("Group", "Year"))

  print(data)
}

# do for 100
new_data <- lapply(1:10, generate_data, analysis = analysis)

new_analyses <- analyse(model, data = new_data)

coef <- coef(analysis)
new_coefs <- lapply(new_analyses, coef)

new_coefs %<>% bind_rows()

new_coefs %<>% select(term, estimate, lower, upper)
coef %<>% select(term, old_estimate = estimate)

new_coefs %<>% inner_join(coef, by = "term")

new_coefs %<>% mutate(within = old_estimate >= lower & old_estimate <= upper)

new_coefs %<>% group_by(term) %>%
  summarise(estimate = mean(estimate), lower = mean(lower), upper = mean(upper),
            old_estimate = first(old_estimate),
            within = sum(within) / n(), n = n()) %>%
  ungroup()

write_csv(new_coefs, "output/tables/group-validation.csv")
