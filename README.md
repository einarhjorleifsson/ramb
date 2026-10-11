
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ramb

<!-- badges: start -->

<!-- badges: end -->

The goal of {ramb} is to return various “move”-objects to tibbles for
further downstream processing using the tidyverse lingo. The focus has
been on AIS vessel data and has only been tested on limited dataset.

The multibeam-bathymetry raster and rayshading functions that used to
live here moved to the sister package {omb} (`../omb`, functions
prefixed `om_`) in 2026-10.

## Installation

You can install the development version of {ramb} from
[GitHub](https://github.com/einarhjorleifsson/ramb) with:

``` r
# install.packages("devtools")
devtools::install_github("einarhjorleifsson/ramb")
```

## Example

``` r
library(ramb)
```

``` r
# great-circle distance in metres
rb_calc_distance(-21.93, 64.15, -23.93, 65.65)
#> [1] 191601.4
```

``` r
# the ICES statistical rectangle of a position
rb_encode_ices_rectangle(c(-10.5, 15.2), c(62.5, 54.9))
#> [1] "54D9" "38G5"
```
