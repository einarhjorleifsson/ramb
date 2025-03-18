#' Rayshayd
#'
#' @param r A terra raster
#' @param zscale Value (default 1) passed to rayshader
#' @param zrange In NULL (default) no scaling
#' @return A rgb terra raster
#' @export 
#'
mb_rayshade_raster_rgb <- function(r, zscale = 1, zrange = NULL) {
  
  # determine split ------------------------------------------------------------
  dr <- ceiling(nrow(r) / 4000)
  dc <- ceiling(ncol(r) / 4000)
  dr  <- ceiling(nrow(r) / dr)
  dc  <- ceiling(ncol(r) / dc)
  agg <- terra::aggregate(r, fact = c(dr, dc))
  
  if(!dir.exists(paste0(tempdir(), "/in"))) {
    dir.create(paste0(tempdir(), "/in"))
  } else {
    ls <- dir(paste0(tempdir(), "/in/"), full.names = T)
    file.remove(ls)
  }
  
  if(!dir.exists(paste0(tempdir(), "/out"))) {
    dir.create(paste0(tempdir(), "/out"))
  } else {
    ls <- dir(paste0(tempdir(), "/out/"), full.names = T)
    file.remove(ls)
  }
  
  filename_in <- paste0(tempdir(), "/in/", "_.tif")
  
  # remove tiles that are empty
  files_in <- terra::makeTiles(r, agg, filename_in, na.rm = TRUE)
  
  print(paste0("Number of tiles: ", length(files_in)))
  
  for(i in 1:length(files_in)) {
    print(paste0("Tile ", i, " of ", length(files_in)))
    org <- terra::rast(files_in[i])
    v <- terra::values(org) |> as.vector()
    if(sum(!is.na(v))) {
      if(!is.null(zrange)) {
        org[1] <- zrange[1]
        org[2] <- zrange[2]
      }
      org <-
        org |>
        mb_rayshade_raster(zscale = zscale) |>
        mb_rayshade_to_rgb(org)
      org |>
        terra::writeRaster(filename = paste0(tempdir(), "/out/rs_", i, ".tiff"))
    }
  }
  ## seam rayshaded tiles back together ----------------------------------------
  files_out <- dir(paste0(tempdir(), "/out"), full.names = TRUE)
  if(length(files_out) == 1) {
    rs <- terra::rast(files_out)
  } else {
    rs <- terra::merge(terra::sprc(files_out))
  }
  return(rs)
}
