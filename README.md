
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iiptreviewer <img src="man/figures/logo.png" width = "160" align="right"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/pedscience/iiptreviewer/workflows/R-CMD-check/badge.svg)](https://github.com/pedscience/iiptreviewer/actions)
<!-- badges: end -->

The goal of iiptreviewer is to make the underlying data as accessible as
possible to reproduce the analyses or use them for one’s own research.

## Installation

You can install the released version of iiptreviewer from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("benediktclaus/iiptreviewer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(iiptreviewer)

iipt_dataset
#> # A tibble: 161 x 27
#>    study_id study_label    year intervention_id country domain   description    
#>       <dbl> <glue>        <dbl>           <dbl> <chr>   <chr>    <chr>          
#>  1        8 Dobe et al.,~  2011               3 Germany Pain in~ Mean pain inte~
#>  2        8 Dobe et al.,~  2011               3 Germany Pain in~ Mean pain inte~
#>  3        8 Dobe et al.,~  2011               3 Germany Pain in~ Mean pain inte~
#>  4        8 Dobe et al.,~  2011               3 Germany Pain in~ Mean pain inte~
#>  5        8 Dobe et al.,~  2011               3 Germany Physica~ Pain-related d~
#>  6        8 Dobe et al.,~  2011               3 Germany Physica~ Pain-related d~
#>  7        8 Dobe et al.,~  2011               3 Germany Physica~ Pain-related d~
#>  8        8 Dobe et al.,~  2011               3 Germany Role fu~ Missed school ~
#>  9        8 Dobe et al.,~  2011               3 Germany Role fu~ Missed school ~
#> 10        8 Dobe et al.,~  2011               3 Germany Role fu~ Missed school ~
#> # ... with 151 more rows, and 20 more variables: instrument <chr>,
#> #   outcome <chr>, type <chr>, analysis_time <chr>, time_1 <chr>, time_2 <chr>,
#> #   mean_1 <dbl>, mean_2 <dbl>, sd_1 <dbl>, sd_2 <dbl>, n_pairs <dbl>,
#> #   n_1 <dbl>, n_2 <dbl>, correlation <dbl>, g <dbl>, var_g <dbl>, se_g <dbl>,
#> #   r <dbl>, var_r <dbl>, se_r <dbl>
```
