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
#' @param tooltip the variable to show as tooltip
#' @param add_harbour add harbour (default TRUE)
#' @param no_lines plot track as lines (default = TRUE)
#' @param highlight_colour colour of track (default black)
#' @param stroke_colour stroke colour of track (default cyan)
#' 
#'
#' @return a mapdeck map
#' @export
#'
rb_mapdeck <- 
  function(d, col = "speed", tooltip = "speed", add_harbour = TRUE, no_lines = TRUE,
           highlight_colour = "black", stroke_colour = "cyan") {
    
    if(add_harbour) hb <- gisland::gl_read_is_harbours(trim = FALSE)
    
    if(!"sf" %in% class(d)) {
      d <- 
        d |> 
        sf::st_as_sf(coords = c("lon", "lat"),
                     crs = 4326)
    }
    
    if(!no_lines) {
      track <- 
        d |> 
        summarise(do_union = FALSE) |> 
        sf::st_cast("LINESTRING")
    }
    
    m <- mapdeck::mapdeck() 
    if(add_harbour) {
      m <- 
        m |> 
        mapdeck::add_polygon(data = hb,
                             fill_colour = col2hex("pink"),
                             layer_id = "harbour")
    }
    
    if(!no_lines) {
      m <- 
        m |> 
        mapdeck::add_path(data = track,
                          layer_id = "track",
                          stroke_width = 300,
                          width_min_pixels = 1,
                          width_max_pixels = 5,
                          #tooltip = "vessel",
                          auto_highlight = TRUE,
                          highlight_colour =  paste0(col2hex(highlight_colour), "80"),
                          update_view = FALSE,
                          stroke_colour = paste0(col2hex(stroke_colour), "80"))
    }
    
    m |> 
      mapdeck::add_scatterplot(data = d,
                               fill_colour = col,
                               radius = 400,
                               tooltip = tooltip,
                               layer_id = "points",
                               palette = "inferno",
                               legend = TRUE)
    
  }
