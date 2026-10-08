# geodesic.R — distance on the WGS84 ellipsoid, recoded from traipse/geodist (plan 013: no copied code).
#
# Vincenty's inverse formula. It agrees with geodist's "geodesic" measure (Karney) to well under a millimetre for
# the short steps of a track; it may not converge for nearly antipodal points, which successive pings never are.

.rb_geodesic <- function(lon1, lat1, lon2, lat2) {
  a <- 6378137; f <- 1 / 298.257223563; b <- (1 - f) * a; k <- pi / 180
  L <- (lon2 - lon1) * k
  U1 <- atan((1 - f) * tan(lat1 * k)); U2 <- atan((1 - f) * tan(lat2 * k))
  sU1 <- sin(U1); cU1 <- cos(U1); sU2 <- sin(U2); cU2 <- cos(U2)
  lam <- L
  for (i in 1:200) {
    sl <- sin(lam); cl <- cos(lam)
    ss <- sqrt((cU2 * sl)^2 + (cU1 * sU2 - sU1 * cU2 * cl)^2)
    cs <- sU1 * sU2 + cU1 * cU2 * cl
    sig <- atan2(ss, cs)
    sa <- ifelse(ss == 0, 0, cU1 * cU2 * sl / ss)
    c2a <- 1 - sa^2
    c2m <- ifelse(c2a == 0, 0, cs - 2 * sU1 * sU2 / c2a)
    C <- f / 16 * c2a * (4 + f * (4 - 3 * c2a))
    lam0 <- lam
    lam <- L + (1 - C) * f * sa * (sig + C * ss * (c2m + C * cs * (-1 + 2 * c2m^2)))
    if (all(abs(lam - lam0) < 1e-12, na.rm = TRUE)) break
  }
  u2 <- c2a * (a^2 - b^2) / b^2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
  ds <- B * ss * (c2m + B / 4 * (cs * (-1 + 2 * c2m^2) - B / 6 * c2m * (-3 + 4 * ss^2) * (-3 + 4 * c2m^2)))
  b * A * (sig - ds)
}

# The step from the previous fix: NA for the first. As traipse::track_distance(), track_time(), track_speed().
.rb_track_distance <- function(x, y) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  c(NA_real_, .rb_geodesic(x[-n], y[-n], x[-1], y[-1]))
}
.rb_track_time <- function(date) {
  if (!inherits(date, "POSIXct")) date <- as.POSIXct(date)
  if (length(date) == 0) return(numeric(0))
  c(NA_real_, diff(unclass(date)))
}
.rb_track_speed <- function(x, y, date) .rb_track_distance(x, y) / .rb_track_time(date)
