## code to prepare `DATASET` dataset goes here
library(data.table)
library(mapdeck)
library(sf)
library(lubridate)
library(tidyverse)
library(mar)
con <- connect_mar()
# source("TOPSECRET.R")

# Jeppe's data -----------------------------------------------------------------
dansk <- 
  "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv" |> 
  readr::read_csv() |> 
  dplyr::distinct(vessel_id, time_stamp, .keep_all = TRUE) |> 
  dplyr::arrange(vessel_id, time_stamp) |> 
  dplyr::mutate(.rowid = 1:dplyr::n(),
                .before = vessel_id)
usethis::use_data(dansk, overwrite = FALSE)

dansk_harbours <- 
  readRDS("~/stasi/ices/WKSSFGEO/data/harbours.rds") |> 
  dplyr::mutate(SI_HARB = 1:dplyr::n())
usethis::use_data(dansk_harbours, overwrite = FALSE)




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

# Whacky 1 ---------------------------------------------------------------------
whacks <- c(5, 10, 11, 15, 16, 17, 27:31)
whacks1 <-
  tibble(time = seq(ymd_hms("2022-10-15 00:00:00"),
                    ymd_hms("2022-10-15 00:45:00"),
                    by = "60 sec")) |> 
  mutate(vid = 1,      # vessel id
         tid = 1,      # trip id
         .rid = 1:n(), # unique number
         lat = seq(62, 62.2, length.out = max(.rid)),
         lon = -25,
         lon = ifelse(.rid %in% whacks, -24.5, lon),
         whacks = ifelse(.rid %in% whacks, TRUE, FALSE),
         lat = ifelse(.rid == 40, 62.04, lat),
         lon = ifelse(.rid == 40, -25.04, lon),
         whacks = ifelse(.rid == 40, TRUE, whacks),
         speed = traipse::track_speed(lon, lat, time)) |> 
  select(vid, tid, .rid, lon, lat, time, speed, whacks) |> 
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4326,
               remove = FALSE) |> 
  sf::st_transform(crs = 3057) %>%
  bind_cols(sf::st_coordinates(.) |> as_tibble() |> janitor::clean_names()) |> 
  st_drop_geometry() |> 
  select(.rid, vid, tid, lon, lat, time, whacks)
usethis::use_data(whacks1)



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

is_survey_station <- 
  les_stod(con) %>% 
  left_join(les_syni(con)) %>% 
  filter(synaflokkur_nr %in% c(30, 35)) %>%
  select(cruise_id = leidangur_id, 
         station_id = stod_id,
         id = synis_id,
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
  mutate(date = as_date(date))

is_survey_station %>% 
  write_csv("/home/ftp/pub/data/csv/is_survey-stations.csv")
system("chmod a+rX /home/ftp/pub/data/csv/is_survey-stations.csv")
tows <- read_csv("ftp://ftp.hafro.is/pub/data/csv/is_survey-stations.csv")

tows <- 
  # read_csv("ftp://ftp.hafro.is/pub/data/csv/is_survey-stations.csv") %>% 
  is_survey_station %>% 
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
         # was 2, trying 3
         T12 = T2 + days(3)) %>% 
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
  ramb:::rb_interval_id2(cruises, vid, time, T11, T12, cruise_id) %>% 
  rename(cruise_id = .id)
d <- 
  d %>% 
  filter(!is.na(cruise_id)) %>% 
  select(vid, time, lon, lat, speed, heading, cruise_id, rectime)
# start series in harbour, end series in harbour
hb <- ramb::read_is_harbours() %>% mutate(inharbour = TRUE)
d2 <-
  d %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) %>% 
  st_join(hb %>% select(inharbour)) %>% 
  st_drop_geometry()
d2.sum <-
  d2 %>% 
  group_by(cruise_id) %>% 
  summarise(inport = which(inharbour, TRUE)) %>% 
  summarise(first = min(inport),
            last = max(inport))
trail <- 
  d2 %>% 
  group_by(cruise_id) %>% 
  mutate(n = n(),
         .rid = 1:n()) %>% 
  ungroup() %>% 
  left_join(d2.sum) %>% 
  filter(.rid >= first, .rid <= last) %>% 
  select(-c(inharbour:last))
trail <- 
  trail %>% 
  distinct(vid, time, .keep_all = TRUE) %>% 
  rename(.vid = vid) %>% 
  ramb::rb_interval_id2(tows %>% rename(.vid = vid) %>% filter(!is.na(t2)),cruise_id, time, t1, t2, id) %>% 
  rename(id = .id) %>% 
  # should not really need to do this
  distinct(vid, time, .keep_all = TRUE) %>% 
  select(-rectime)

trail %>%   
  write_csv("/home/ftp/pub/data/csv/is_survey-tracks.csv")
system("chmod a+rX /net/www/export/home/ftp/pub/data/csv/is_survey-tracks.csv")
trail <- read_csv("ftp://ftp.hafro.is/pub/data/csv/is_survey-tracks.csv")


# Benthis parameters -----------------------------------------------------------
benthis_parameters <- 
  icesVMS::get_benthis_parameters() |> 
  dplyr::as_tibble() |> 
  dplyr::select(benthis_metier = benthisMet,
                a = firstFactor,
                b = secondFactor,
                model = gearModel,
                variable = gearCoefficient,
                subsurface_percentage = subsurfaceProp,
                dplyr::everything()) |> 
  janitor::clean_names()
usethis::use_data(benthis_parameters, overwrite = TRUE)

# Benthis - metier5 lookup -----------------------------------------------------
metier5_benthis_lookup <- 
  "https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv" |> 
  read.csv() |> 
  dplyr::mutate(metier5 = ramb::rb_met5_from6(Metier_level6)) |> 
  dplyr::select(metier5, benthis_metier = Benthis_metiers) |> 
  tibble::as_tibble() |> 
  dplyr::filter(benthis_metier != "") |>
  dplyr::distinct()
metier5_benthis_lookup |> usethis::use_data()
