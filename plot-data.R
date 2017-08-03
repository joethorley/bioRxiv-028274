source("header.R")

dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

pdo <- readRDS("output/clean/pdo.rds")
leks <- readRDS("output/clean/leks.rds")
wells <- readRDS("output/clean/wells.rds")
groups <- readRDS("output/clean/groups.rds")

print(ggplot(data = pdo, aes(x = Year, y = PDO)) +
        geom_line() +
        scale_y_continuous(name = "PDO Index"))

ggsave("output/plots/pdo-data.png", width = 3, height = 2, dpi = dpi)

leks %<>% st_fortify()
centroids <- groups %>% st_centroid() %>% st_fortify()
groups %<>% st_fortify()
wells %<>% st_fortify()

print(ggplot(data = leks, aes(x = x/1000, y = y/1000)) +
        geom_point(data = wells, size = 1/20, alpha = 1/3, color = "grey") +
        geom_point(size = 1/5, alpha = 1/2, color = "blue") +
        geom_path(data = groups, aes(group = group), color = "black") +
        geom_text(data = centroids, aes(label = Group), color = "black",  size = 4) +
        coord_fixed() +
        scale_x_continuous("Easting (km)", labels = comma) +
        scale_y_continuous("Northing (km)", labels = comma) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

ggsave("output/plots/wyoming.png", width = 6, height = 5, dpi = dpi)
