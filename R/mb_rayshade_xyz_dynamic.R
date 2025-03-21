#' Dynamic rayshading
#'
#' @param xyz xxx
#' @param r0 xxx
#' @param ping_cutoff xxx
#' @param file_prefix xxx 
#' @param max_meters Default 1024
#'
#' @return A raster
#' @export
#'
mb_rashade_xyz_dynamic <- function(xyz,
                                   r0,
                                   ping_cutoff = 10,
                                   file_prefix = "tmp/tmp",
                                   max_meters = 1024
                                   
) {
  
  # overly complex
  res <- terra::res(r0)[1]
  i = 2 ^ (1:14)  # 8, 16, 32, 64, 128, 256, 512 meters
  m <- terra::res(r0)[1]
  meters <- i[i >= m & i <= max_meters]
  agg <- meters / min(meters)
  # ensure extent
  r0 <- 
    r0 |> 
    terra::aggregate(max(meters) / min(meters)) |> 
    terra::disagg(max(meters) / min(meters))
  
  tfile <- paste0("tmp/r_", sample(1:1000, 1), ".tif")
  
  for(i in 1:length(meters)) {
    print(paste0("resolution ", i, " (", meters[i], " meters) of ", length(meters), " (", meters[length(meters)], " m)"))
    R <-
      xyz |>
      ramb::mb_rasterize_xyz(r0, fun = "mean", agg = agg[i])
    Rc <-
      xyz |>
      ramb::mb_rasterize_xyz(r0, fun = "count", agg = agg[i])
    # if fewer than X measures drop rayshading, use next level up
    v <- values(Rc)
    # check if there are any values to rayshade
    RS <-
      R |>
      ramb::mb_rayshade_raster_rgb(zscale = res[i])
    if(i < max(res)) RS[v < ping_cutoff] <- NA
    # some fix, something about "in memory" or not
    if(i == 1) {
      terra::writeRaster(RS, tfile, overwrite = TRUE)
      RS <- terra::rast(tfile)
    }
    if(i > 1) RS <- RS |> terra::disagg(agg[i])
    if(i == 1) e <- terra::ext(RS)
    RS <- terra::crop(RS, e)
    terra::writeRaster(RS, paste0(file_prefix, "_r", res[i], ".tif"), overwrite = TRUE)
  }
  print("Doing cover")
  # Need to make this dynamic
  r <-
    terra::rast(paste0(file_prefix, "_r", res[1], ".tif")) |>
    terra::cover(terra::rast(paste0(file_prefix, "_r", res[2], ".tif"))) |>
    terra::cover(terra::rast(paste0(file_prefix, "_r", res[3], ".tif"))) |>
    terra::cover(terra::rast(paste0(file_prefix, "_r", res[4], ".tif"))) |>
    terra::cover(terra::rast(paste0(file_prefix, "_r", res[5], ".tif")))
  return(r)
}