
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sysrevdata

<!-- badges: start -->
<!-- [![Travis build status](https://travis-ci.com/softloud/sysrevdata.svg?branch=master)](https://travis-ci.com/softloud/sysrevdata) -->
<!-- badges: end -->

The goal of sysrevdata is to provide examples and extensible toolchains
for structuring data for systematic review.

See vignettes for example toolchain walkthroughs.

## Vignettes

-   Narrative synthesis table \| Condensed summaries of data for human
    interpretation
-   Long-form data for complex visualisations and analyses \|
    Structuring data for machine interpretation

## Example dataset

This package currently contains `bufferstrips` dataset from a published
systematic review.

``` r
library(sysrevdata)

# take a peek at the first 10 rows, 
# restricted to first 5 columns, 
# of each dataset

bufferstrips %>%
  # select first five columns
  dplyr::select(1:5) %>% 
  # select first 10 rows
  head(10)
#> # A tibble: 10 x 5
#>     item_id short_title    title                                    year period 
#>       <dbl> <chr>          <chr>                                   <dbl> <chr>  
#>  1 20641367 Aaron (2005)   Invertebrate Biodiversity in Agricultu…  2005 2005-2…
#>  2 20641374 Aavik (2008)   What is the role of local landscape st…  2008 2005-2…
#>  3 20641375 Aavik (2010)   Quantifying the effect of organic farm…  2010 2010-2…
#>  4 20641382 Abu-Zreig (20… Experimental investigation of runoff r…  2004 2000-2…
#>  5 20641384 Abu-Zreig (20… Phosphorus removal in vegetated filter…  2003 2000-2…
#>  6 20641386 Adams (2013)   Disturbance and landscape effects on a…  2013 2010-2…
#>  7 20641389 Ahern (2002)   Effect of different wheat production s…  2002 2000-2…
#>  8 20641390 Al (2013)      Does the presence of grassy strips and…  2013 2010-2…
#>  9 20641397 Alignier (201… Changes in management practices over t…  2015 2015-2…
#> 10 20641398 Anan'eva (200… Comparative assessment of soil microbi…  2008 2005-2…
```

See

-   `?bufferstrips`

for more details.

## Installation

You can install
<!-- the released version of sysrevdata from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->
<!-- install.packages("sysrevdata") -->
<!-- ``` -->
<!-- And  -->

the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("softloud/sysrevdata")
```

# Contributing to this

This is work that extended from ideas generated at the [Evidence
Synthesis Hackathon](https://www.eshackathon.org/); it is the voluntary
work of open scientists who welcome contributions. If you have an idea
for an improvement or extension, please consider contributing a pull
request or opening an
[issue](https://github.com/softloud/sysrevdata/issues).
