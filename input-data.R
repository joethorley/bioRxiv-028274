# input data to R and save as .rds files
source("header.R")

dir.create("output/data", showWarnings = FALSE, recursive = TRUE)

pdo <- rpdo::pdo

saveRDS(pdo, "output/data/pdo.rds")

wyoming <- rbind(c(-111.05, 41), c(-111.05, 45), c(-104.05, 45), c(-104.05, 41), c(-111.05, 41)) %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = 4326)

saveRDS(wyoming, "output/data/wyoming.rds")

groups <- read_sf("data/groups")
saveRDS(groups, "output/data/groups.rds")

counts <- read_xlsx("data/grouse/SG Lek Observations 1948-2016.xlsx")
saveRDS(counts, "output/data/counts.rds")

leks <- read_xlsx("data/grouse/SG Lek Locations & Descriptors 2016.xlsx")
saveRDS(leks, "output/data/leks.rds")

# conventional downloaded from
# "http://pathfinder.geospatialhub.org/datasets/97bf34ef9d6b4c1ea2d8a543fa6c56d9_1.zip"

unzip("data/wells/conventional.zip", exdir = "output/data/conventional")


unzip("data/wells/coalbed.zip", exdir = "output/data/coalbed")
unzip("data/wells/injection.zip", exdir = "output/data/injection")

conventional <- read_sf("output/data/conventional")
saveRDS(conventional, "output/data/conventional.rds")

coalbed <- read_sf("output/data/coalbed")
saveRDS(coalbed, "output/data/coalbed.rds")

injection <- read_sf("output/data/injection")
saveRDS(injection, "output/data/injection.rds")
