## code to prepare `DATASET` dataset goes here
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
