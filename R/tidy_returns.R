#' @export
tidy_normalmixEM <- function(m) {
  if(class(m) != "mixEM") stop("The object is not of class 'mixEM'")
  if(m$ft != "normalmixEM") stop("The object is not of class 'normalmixEM'")
  n.modes <- length(m$mu)
  est <- 
    tibble::tibble(modes = 1:n.modes,
                   mu = m$mu,
                   lamda = m$lambda,
                   sigma = m$sigma)
  
  pos <- 
    tidyr::expand_grid(x = m$x,
                       modes = 1:n.modes) %>% 
    dplyr::left_join(est) %>% 
    dplyr::mutate(y = plotmm::plot_mix_comps(x, mu, sigma, lamda, normal = TRUE)) %>% 
    dplyr::arrange(modes)
  
  return(pos)
  
}

# # usage
# prepdat <- 
#   read.table("doi_10.5061_dryad.k80bp46__v1/example_data.txt",
#              header=TRUE, sep=",") %>% 
#   # use the prepData function to create step lengths
#   moveHMM::prepData(type = "UTM", coordNames = c("x", "y")) %>% 
#   filter(ID == "AR001") %>% 
#   filter(!is.na(step)) %>% 
#   # convert step length from meters to knots (nautical miles per hour)
#   mutate(speed = step/ 60 * 1.9438)
# normalmixEM(prepdat$speed, mu = c(1,4,8), sigma = c(1,1,1)) %>%
#   tidy_normalmixEM() %>%
#   ggplot() +
#   geom_density(aes(x = x)) +
#   geom_line(aes(x, y, colour = factor(modes)))

#' @export
tidy_bin_clst_path <- function(o) {
  o@pth %>% 
    dplyr::mutate(spn = o@spn,
                  dst = o@dst,
                  hdg = o@hdg,
                  W = o@W,
                  A = o@A,
                  turn = EMbC:::getTurns(o),
                  speed = EMbC:::getSpeed(o)) %>% 
    tibble::as_tibble()
}

# # usage
# d <- 
#   read.table("doi_10.5061_dryad.k80bp46__v1/example_data.txt",
#              header=TRUE, sep=",") %>% 
#   mutate(time = lubridate::ymd_hms(date)) %>% 
#   # use the prepData function to create step lengths
#   moveHMM::prepData(type = "UTM", coordNames = c("x", "y")) %>% 
#   # check if there are any missing step lengths
#   # there will be at least five (=the number of unique IDs) because the first step
#   # length of each trip will be NA
#   # remove missing step lengths
#   filter(!is.na(step)) %>% 
#   # convert step length from meters to knots (nautical miles per hour)
#   mutate(speed = step / 60 * 1.9438) %>% 
#   as_tibble() %>% 
#   sf::st_as_sf(coords = c("x", "y"),
#                crs = '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0',
#                remove = FALSE) %>% 
#   sf::st_transform(crs = 4326) %>% 
#   mutate(lon = sf::st_coordinates(geometry)[,1],
#          lat = sf::st_coordinates(geometry)[,2]) %>% 
#   sf::st_drop_geometry()
# o <- 
#   d %>% 
#   filter(ID == "AR001") %>% 
#   select(time, lon, lat) %>% 
#   as.data.frame() %>% 
#   stbc(., info = -1)
# tidy_bin_clst_path(o)
