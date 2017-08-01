# read all data .rds files into R and clean up
source("header.R")

dir.create("output/clean", showWarnings = FALSE, recursive = TRUE)

pdo <- readRDS("output/data/pdo.rds")
wyoming <- readRDS("output/data/wyoming.rds")
groups <- readRDS("output/data/groups.rds")
leks <- readRDS("output/data/leks.rds")
counts <- readRDS("output/data/counts.rds")

conventional <- readRDS("output/data/conventional.rds")
coalbed <- readRDS("output/data/coalbed.rds")
injection <- readRDS("output/data/injection.rds")

wells <- list(conventional = conventional,
              coalbed = coalbed,
              injection = injection)

rm(conventional, coalbed, injection)

pdo %<>%
  group_by(Year) %>%
  summarise(PDO = mean(PDO)) %>%
  ungroup() %>%
  filter(Year %in% (first_year - max(lags)):last_year)

wyoming %<>%
  st_transform("+init=epsg:26913")

groups %<>%
  st_as_sf() %>%
  st_transform("+init=epsg:26913") %>%
  select(Group = NameStanda) %>%
  mutate(Group = str_replace(Group, "Rive$", "River"))

leks %<>%
  filter(!is.na(Lon),!is.na(Lat)) %>%
  select(Lek = LekID, Lon, Lat) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform("+init=epsg:26913") %>%
  st_crop(wyoming) %>%
  st_join(groups) %>%
  filter(!is.na(Group)) %>%
  filter(Group != "Upper Snake River")

counts %<>%
  rename(Lek = LekID, Males = Male) %>%
  mutate(Date = as.Date(str_c(Year, Month, Day, sep = "-"))) %>%
  filter(`Check?` == "Yes", Method == "Ground", Type %in% c("Count", "Survey")) %>%
  filter(!is.na(Date)) %>%
  mutate(Dayte = dayte(Date), Year = year(Date), Males = as.integer(Males),
         Year = as.integer(Year)) %>%
  filter(Year %in% first_year:last_year) %>%
  filter(Dayte >= as.Date("1972-04-01"), Dayte <= as.Date("1972-05-07")) %>%
  filter(!is.na(Males)) %>%
  filter(is.na(Unk) | (Unk < Males / 20))

counts %<>% semi_join(leks, by = "Lek")
leks %<>% semi_join(counts, by = "Lek")

# convert wells to a single sf (simple features object)
wells %<>%
  lapply(st_as_sf) %>%
  lapply(select, SpudDate = SPUD, Well = API_NUMBER) %>%
  reduce(rbind)

wells %<>%
  st_transform("+init=epsg:26913") %>%
  st_crop(wyoming)

wells %<>% mutate(SpudDate = as.character(SpudDate)) %>%
  filter(SpudDate != "0")

# convert SpudDate text to an actual date (1st of month)
wells %<>%
  mutate(SpudDate = str_c(SpudDate, "01"), SpudDate = ymd(SpudDate)) %>%
  filter(!is.na(SpudDate)) %>%
  filter(year(SpudDate) %in% 1900:last_year)

start_date <- str_c((first_year - max(lags)), "-01-01") %>% as.Date()
wells$SpudDate[wells$SpudDate < start_date] <- start_date

wells %<>% mutate(Year = year(SpudDate)) %>%
  select(-SpudDate) %>%
  arrange(Year, Well)

saveRDS(pdo, "output/clean/pdo.rds")
saveRDS(wyoming, "output/clean/wyoming.rds")
saveRDS(groups, "output/clean/groups.rds")
saveRDS(leks, "output/clean/leks.rds")
saveRDS(counts, "output/clean/counts.rds")
saveRDS(wells, "output/clean/wells.rds")
