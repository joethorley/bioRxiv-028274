dayte <- function(x){
  x %<>% date()
  year(x) <- 1972
  x
}

st_crop <- function(x, y) {
  bbox <- st_bbox(y)
  crs <- st_crs(bbox)
  bbox <- c(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
  bbox %<>%
    raster::extent() %>%
    as("SpatialPolygons") %>%
    st_as_sf() %>%
    st_set_crs(crs)
  x %<>% st_intersection(bbox)
  x
}

st_fortify <- function(x) {
  if (!nrow(x)) {
    x %<>%
      as.data.frame() %>%
      select(-geometry) %>%
      mutate(x = numeric(0), y = numeric(0))
    return(x)
  }
  x %<>% as("Spatial")
  if (inherits(x, "SpatialPointsDataFrame")) {
    x %<>%
      as.data.frame() %>%
      rename(long = coords.x1, lat = coords.x2)
  } else
    x %<>% fortify()
  x %<>%
    as.tbl() %>%
    rename(x = long, y = lat)
  x
}

accumulate_wells <- function(x, first_year, last_year) {
  x %<>% count(Year) %>%
    left_join(data_frame(Year = first_year:last_year), ., by = "Year") %>%
    replace_na(list(n = 0)) %>%
    arrange(Year) %>%
    mutate(Wells = cumsum(n)) %>%
    select(-n) %>%
    as.tbl()
  x
}

trim_males <- function(y, min_years, last_year) {
  y %<>% filter(!is.na(Males))
  y %<>% arrange(Year)
  which <- which(diff(y$Year) > 1) %>% add(1L)
  if (length(which))
    y %<>% slice(-seq_len(max(which)))
  if (nrow(y) < min_years || max(y$Year) != last_year)
    return(NULL)
  y
}

st_intersection_switch <- function(x,y) st_intersection(y,x)

exp_minus1 <- function(x) exp(x) - 1

add_ABC <- function(x) {
  if (!length(x)) return(x)

  letters <- letters[1:nlevels(x)] %>%
    toupper() %>%
    paste0("(", ., ")")
  levels(x) %<>% paste(letters, .)
  x
}

