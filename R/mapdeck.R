# should add the alpa/transparency here
col2hex <- function (cname) {
  colMat <- col2rgb(cname)
  rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3, 
  ]/255)
}

#' Plot trails
#'
#' @param d a tibble containing trails
#' @param col the variable that controls colour
#'
#' @return a mapdeck map
#' @export
#'
rb_mapdeck <- function(d, col = "speed", add_harbour = TRUE) {
  
  if(add_harbour) hb <- gisland::gl_read_is_harbours(trim = FALSE)
  
  if(!"sf" %in% class(d)) {
    d <- 
      d |> 
      sf::st_as_sf(coords = c("lon", "lat"),
                   crs = 4326)
  }
  
  track <- 
    d |> 
    summarise(do_union = FALSE) |> 
    sf::st_cast("LINESTRING")
  
  m <- mapdeck::mapdeck() 
  if(add_harbour) {
    m <- 
      m |> 
      mapdeck::add_polygon(data = hb,
                           fill_colour = col2hex("pink"),
                           layer_id = "harbour")
  }
  m |> 
    mapdeck::add_path(data = track,
                      layer_id = "track") |> 
    mapdeck::add_scatterplot(data = d,
                             fill_colour = col,
                             radius = 400,
                             tooltip = "speed",
                             layer_id = "points",
                             palette = "inferno",
                             legend = TRUE)
    
}
