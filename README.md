
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matrixmodel

<!-- badges: start -->

<!-- badges: end -->

The goal of the Indiana Forest Simulator (AKA matrixmodel) is to provide
Indiana foresters with a tool to simulate forest stocks in the State
into the future.

## Installation

You can install the development version of matrixmodel from
[GitHub](https://github.com/) by typing the following in the R console:

``` r
# install.packages("pak")
pak::pak("panchorivasf/matrixmodel")
```

Once the package is installed, load it into `R` with:

``` r
library(matrixmodel)
```

To start, you need to search for a CSV file in the ‘matrix model’ format
and import it into `R`:

``` r
setwd("path/to/your/folder")
```
