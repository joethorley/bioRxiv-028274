source("header.R")

# download well data

conventional <- "http://pathfinder.geospatialhub.org/datasets/97bf34ef9d6b4c1ea2d8a543fa6c56d9_1.zip"

download.file(conventional, "data/wells/conventional.zip")

coalbed <- "http://pathfinder.geospatialhub.org/datasets/97bf34ef9d6b4c1ea2d8a543fa6c56d9_2.zip"

download.file(coalbed, "data/wells/coalbed.zip")

injection <- "http://pathfinder.geospatialhub.org/datasets/97bf34ef9d6b4c1ea2d8a543fa6c56d9_3.zip"

download.file(injection, "data/wells/injection.zip")
