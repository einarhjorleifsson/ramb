## code to prepare `DATASET` dataset goes here

# The creel data ---------------------------------------------------------------
prj <- '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
creel <-
  read.table("data-raw/doi_10.5061_dryad.k80bp46__v1.txt",
             header=TRUE, sep=",") %>%
  as_tibble() %>%
  select(id = ID, time = date, x, y, behaviour) %>%
  mutate(id = ifelse(id == "GP004", "GP005", id),
         id = as.integer(stringr::str_sub(id, 5)),
         time = lubridate::ymd_hms(time)) %>%
  sf::st_as_sf(coords = c("x", "y"),
               crs = prj,
               remove = FALSE) %>%
  sf::st_transform(crs = 4326) %>%
  mutate(lon = sf::st_coordinates(geometry)[, 1],
         lat = sf::st_coordinates(geometry)[, 2]) %>%
  sf::st_drop_geometry() %>%
  group_by(id) %>%
  mutate(rowid = 1:n()) %>%
  ungroup() %>%
  select(id, rowid, time, x, y, lon, lat, behaviour)
usethis::use_data(creel, overwrite = TRUE)

# Trawl survey -----------------------------------------------------------------
library(tidyverse)
library(data.table)

#library(EMbC)
stk <-
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_vms2019.csv") %>%
  select(vid, lon, lat, speed, heading, time) %>%
  arrange(vid, time) %>%
  drop_na(lon, lat) %>%
  as.data.frame()
lgs <-
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_stations.csv") %>%
  filter(year == 2019) %>%
  arrange(t1, t2) %>%
  select(id.lgs = id, vid, lon.start = lon1, lat.start = lat1,
         lon.end = lon2, lat.end = lat2, start = t1, end = t2)
stk.dt <-
  stk %>%
  arrange(time) %>%
  mutate(dummy = time) %>%
  data.table()
lgs.dt <-
  lgs %>%
  arrange(start, end) %>%
  data.table()
setkey(stk.dt, vid, time, dummy)
setkey(lgs.dt, vid, start, end)
trawlsurvey <-
  foverlaps(stk.dt, lgs.dt, nomatch = NA) %>%
  as_tibble() %>%
  mutate(behaviour = ifelse(!is.na(id.lgs), "hauling", "not hauling")) %>%
  select(vid, lon, lat, speed, heading, time, behaviour)
usethis::use_data(trawlsurvey, overwrite = TRUE)

trawlsurveytows <-
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_stations.csv") %>%
  filter(year == 2019) %>%
  arrange(t1, t2) %>% 
  select(id.lgs = id, vid, lon.start = lon1, lat.start = lat1,
         lon.end = lon2, lat.end = lat2, start = t1, end = t2) %>% 
  select(id.lgs, x1 = lon.start, x2 = lon.end, y1 = lat.start, y2 = lat.end) %>% 
  pivot_longer(-id.lgs,
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)") %>% 
  arrange(id.lgs) %>% 
  sf::st_as_sf(coords = c("x", "y"),
           crs = 4326) %>% 
  group_by(id.lgs) %>% 
  summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING")
usethis::use_data(trawlsurveytows, overwrite = TRUE)
