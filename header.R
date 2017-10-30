library(devtools)
library(foreach)
library(doParallel)
library(ggplot2)
library(ggsn)
library(lubridate)
library(magrittr)
library(newdata)
library(plyr)
library(purrr)
library(raster)
library(readxl)
library(readr)
library(rpdo)
library(rgdal)
library(scales)
library(units)
library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(tmbr)
library(smbr)

rm(list = ls())
graphics.off()

source("functions.R")

if (getDoParWorkers() == 1) {
  message("registering 4 workers")
  registerDoParallel(4)
}

set_analysis_mode("report")

theme_set(theme_grey(base_size = 10))
theme_replace(axis.ticks        = element_line(colour = "black"),
              legend.key        = element_rect(colour = "grey80"),
              legend.title      = element_text(size = rel(0.8)),
              panel.background  = element_rect(fill = "white", colour = NA),
              panel.border      = element_rect(fill = NA, colour = "black"),
              panel.grid.major  = element_line(colour = "grey80", size = 0.5),
              panel.grid.minor  = element_line(colour = "grey90", size = 0.2),
              strip.background  = element_rect(fill = "grey80", colour = "black"))

palette(c("black", "red", "blue", "green4", "orange3", "slategray"))

first_year <- 1985
last_year <- 2016

dpi <- 900L

dists <- c(800, 1600, 3200, 6400)
lags <- 1:4

min_leks <- 25
min_years <- 10

dir.create("output/values", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/data", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tidy", recursive = TRUE, showWarnings = FALSE)
dir.create("output/clean", showWarnings = FALSE, recursive = TRUE)
dir.create("output/group", recursive = TRUE, showWarnings = FALSE)
dir.create("output/lek", recursive = TRUE, showWarnings = FALSE)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("data/wells", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

saveRDS(dists/1000, "output/values/dists.rds")
saveRDS(min_leks, "output/values/min_leks.rds")

