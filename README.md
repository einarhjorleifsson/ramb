
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ramb

<!-- badges: start -->
<!-- badges: end -->

The goal of {ramb} is to return various “move”-objects to tibbles for
further downstream processing using the tidyverse lingo.

## Installation

You can install the development version of {ramb} from
[GitHub](https://github.com/einarhjorleifsson/ramb) with:

``` r
# install.packages("devtools")
devtools::install_github("einarhjorleifsson/ramb")
```

## Example

Basic example of workflow:

``` r
library(tidyverse)
ramb::creel %>% 
  ramb::rb_gaussian() %>% 
  dplyr::glimpse()
#> number of iterations= 17 
#> number of iterations= 25 
#> number of iterations= 50 
#> number of iterations= 22 
#> number of iterations= 69 
#> Rows: 2,221
#> Columns: 14
#> $ id              <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ ID              <chr> "Animal1", "Animal1", "Animal1", "Animal1", "Animal1",…
#> $ step            <dbl> 237.1997, 231.0651, 225.8796, 225.0682, 218.7207, 220.…
#> $ angle           <dbl> NA, -0.002635387, -0.106005617, -0.088730742, 0.056878…
#> $ x               <dbl> 434561.8, 434361.9, 434167.5, 433991.5, 433829.3, 4336…
#> $ y               <dbl> 6451521, 6451648, 6451773, 6451915, 6452071, 6452213, …
#> $ rowid           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ time            <dttm> 2017-05-19 06:50:28, 2017-05-19 06:51:28, 2017-05-19 …
#> $ lon             <dbl> -4.113320, -4.116755, -4.120097, -4.123131, -4.125934,…
#> $ lat             <dbl> 58.20003, 58.20115, 58.20224, 58.20348, 58.20486, 58.2…
#> $ behaviour       <chr> "steaming", "steaming", "steaming", "steaming", "steam…
#> $ speed           <dbl> 7.684481, 7.485738, 7.317747, 7.291460, 7.085823, 7.13…
#> $ threshold.lower <dbl> 0.2430515, 0.2430515, 0.2430515, 0.2430515, 0.2430515,…
#> $ threshold.upper <dbl> 0.8481267, 0.8481267, 0.8481267, 0.8481267, 0.8481267,…
```
