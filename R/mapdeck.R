#' Plot trails
#'
#' @param d a tibble containing trails
#' @param col the variable that controls colour
#'
#' @return a mapdeck map
#' @export
#'
rb_mapdeck <- function(d, col = "speed") {
  
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
  
  mapdeck::mapdeck() |> 
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
