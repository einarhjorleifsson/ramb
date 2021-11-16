## code to prepare `DATASET` dataset goes here
library(data.table)
library(mapdeck)
library(sf)
library(lubridate)
library(tidyverse)
library(mar)
con <- connect_mar()
source("TOPSECRET.R")

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

# All SMB and SMH --------------------------------------------------------------

con <- connect_mar()

les_stod(con) %>% 
  left_join(les_syni(con)) %>% 
  filter(synaflokkur_nr %in% c(30, 35)) %>%
  select(cruise_id = leidangur_id, 
         station_id = stod_id,
         station_nr = stod_nr,
         date = dags,
         vid = skip_nr,
         gid = veidarfaeri,
         sclass = synaflokkur_nr,
         lon1 = kastad_lengd,
         lat1 = kastad_breidd,
         lon2 = hift_lengd,
         lat2 = hift_breidd,
         t1 = togbyrjun,
         t2 = togendir,
         vid = skip_nr) %>% 
  collect(n = Inf) %>% 
  filter(t1 >= ymd_hms("2010-01-01 01:00:00")) %>% 
  mutate(date = as_date(date)) %>% 
  write_csv("/net/www/export/home/ftp/pub/data/csv/is_survey-stations.csv")
system("chmod a+rX /net/www/export/home/ftp/pub/data/csv/is_survey-stations.csv")

tows <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_survey-stations.csv") %>% 
  mutate(t1 = force_tz(t1, "UTC"),
         t2 = force_tz(t2, "UTC"))
cruises <- 
  tows %>% 
  filter(t1 >= ymd_hms("2010-01-01 01:00:00")) %>% 
  group_by(cruise_id, vid) %>% 
  summarise(T1 = min(t1, na.rm = TRUE),
            T2 = max(t2, na.rm = TRUE),
            .groups = "drop") %>% 
  # a buffer around cruise time
  mutate(T11 = T1 - days(2),
         T12 = T2 + days(2)) %>% 
  arrange(T11, T12)

vid.mid.link <- 
  omar:::stk_mobile_icelandic(con, correct = TRUE, vidmatch = TRUE) %>% 
  collect(n = Inf) %>% 
  filter(vid %in% unique(cruises$vid))
mids <- 
  vid.mid.link %>% pull(mid)
trail.raw <- 
  omar::stk_trail(con) %>% 
  filter(mid %in% mids) %>% 
  collect(n = Inf)
trail <- 
  trail.raw %>% 
  filter(time >= ymd_hms("2010-01-01 01:00:00")) %>% 
  distinct(mid, time, .keep_all = TRUE) %>% 
  arrange(time, mid) %>% 
  mutate(rowid = 1:n())
trail <- 
  trail %>% 
  left_join(vid.mid.link %>% 
            select(mid, vid))
d <- 
  trail %>% 
  rb_interval_id2(cruises, vid, time, T11, T12, cruise_id)
d %>% 
  filter(!is.na(cruise_id)) %>% 
  select(vid, time = t, lon, lat, speed, heading, cruise_id) %>% 
  write_csv("/net/www/export/home/ftp/pub/data/csv/is_survey-tracks.csv")
system("chmod a+rX /net/www/export/home/ftp/pub/data/csv/is_survey-tracks.csv")
trail <- read_csv("ftp://ftp.hafro.is/pub/data/csv/is_survey-tracks.csv")
trail

# Harbours ---------------------------------------------------------------------
# not used downstream
library(vmstools)
data(harbours)
harbours.vmstools <- 
  harbours %>% 
  as_tibble() %>% 
  mutate(hid = as.character(1:n()),
         harbour = iconv(harbour, from = "latin1", to = "UTF-8")) %>% 
  select(hid, harbour, lon, lat) %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) %>% 
  # just to create a small polygon
  st_buffer(dist = 100, nQuadSegs = 2) %>% 
  mutate(iso2a = NA_character_) %>% 
  select(hid, harbour, iso2a, lon, lat)


library(omar)
con <- connect_mar()
hb.mfri <- 
  stk_trail(con, 2020) %>% 
  filter(!is.na(hid)) %>% 
  collect(n = Inf) %>% 
  filter(hid != "ISL") %>% 
  distinct(lon, lat, .keep_all = TRUE) %>% 
  filter(io == "I") %>% 
  filter(!hid %in% c("REYF")) %>% 
  mutate(hid = case_when(hid %in% c("HEL", "KEF", "NJA") ~ "KEF",
                         hid %in% c("BAK", "BAK2") ~ "BAK",
                         hid %in% c("MJO", "MJF") ~ "MJO",
                         hid %in% c("REY", "SNF", "GUF") ~ "REY",
                         hid %in% c("KRS", "AKU") ~ "AKU",
                         TRUE ~ hid)) %>% 
  group_by(hid) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  select(-n) %>% 
  mutate(iso2a = case_when(lat > 63 ~ "IS",
                           TRUE ~ "FO"))
median <- 
  hb.mfri %>% 
  group_by(hid) %>% 
  summarise(lon = median(lon),
            lat = median(lat),
            .groups = "drop")

hb.mfri <- 
  hb.mfri %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>% 
  select(hid, iso2a) %>% 
  #filter(hid == "EYB") %>% 
  nest(points = geometry) %>% 
  mutate(mpoints = map(points, st_combine),
         geometry = map(mpoints, st_convex_hull)) %>% 
         #geometry = map(geometry, st_buffer, dist = 1000)) %>% 
  select(hid, iso2a, geometry) %>% 
  unnest(geometry) %>% 
  mutate(harbour = hid) %>% 
  left_join(median) %>%  
  select(hid, harbour, iso2a, lon, lat, geometry)
st_geometry(hb.mfri) <- hb.mfri$geometry

#harbours <- rbind(hb.mfri, harbours.vmstools)
#hb <- harbours %>% filter(harbour == "Dunbar")
#source("TOPSECRET.R")
#mapdeck() %>% add_polygon(data = hb.mfri)



write_sf(hb.mfri, "/net/www/export/home/ftp/pub/data/shapes/harbours.gpkg")
system("chmod a+rX /net/www/export/home/ftp/pub/data/shapes/harbours.gpkg")
harbours <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/harbours.gpkg")

