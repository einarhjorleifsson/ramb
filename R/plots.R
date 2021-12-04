#' rb_md_trip
#'
#' @param data A tibble containing variable names lon and lat (crs 4326)
#' @param tid Trip id variable name, default tid
#' @param radius Radius (in meters) of the point size (default 10)
#' @param col What variable to be used for colour scale (default speed)
#' @param trip add trip path, default FALSE
#'
#' @return xxx
#' 
#' @export
#'
rb_md_trip <- function(data, tid, radius = 10, col = "speed", trip = TRUE) {
  
  if(any(!class(data) %in% "sf")) {
    data <- 
      data %>% 
      sf::st_as_sf(coords = c("lon", "lat"),
                   crs = 4326,
                   remove = FALSE)
  }
  
  m <- mapdeck::mapdeck()
  
  if(trip) {
    m <- 
      m %>% 
      mapdeck::add_path(data = data %>% 
                          dplyr::group_by( {{tid}} ) %>% 
                          dplyr::summarise(do_union = FALSE) %>%
                          sf::st_cast("LINESTRING"),
                        layer_id = "track",
                        stroke_width = 1000,
                        width_min_pixels = 5,
                        width_max_pixels = 10,
                        # NEED TO MAKE THIS DYNAMIC
                        tooltip = "tid",
                        auto_highlight = TRUE,
                        highlight_colour = "#FF000095",
                        update_view = FALSE,
                        stroke_colour = "#E0FFFF80")
  }
  
  m <- 
    m %>% 
    mapdeck::add_scatterplot(data = data,
                             fill_colour = "speed",
                             radius = radius,
                             palette = "inferno")
  return(m)
}