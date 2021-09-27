
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/container)](https://cran.r-project.org/package=container)
[![Build
Status](https://travis-ci.org/rpahl/container.png?branch=master)](https://travis-ci.org/rpahl/container)
[![codecov.io](https://codecov.io/github/rpahl/container/coverage.svg?branch=master)](https://codecov.io/github/rpahl/container?branch=master)
[![status](https://tinyverse.netlify.com/badge/container)](https://CRAN.R-project.org/package=container)
[![Downloads](http://cranlogs.r-pkg.org/badges/container)](http://www.r-pkg.org/pkg/container)
[![R build
status](https://github.com/rpahl/container/workflows/R-CMD-check/badge.svg)](https://github.com/rpahl/container/actions)
<!-- badges: end -->

# container <img src="images/logo.png" align="right" width="163" height="104"/>

container provides common container data structures deque, set, dict
(resembling Python’s dict type) and dict.table (combining dict and
data.table) with typical member functions to insert, delete and access
container elements. Furthermore, provides iterators and supports both
reference and copy semantics.

## Installation

You can install the released version of container from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

## Example

``` r
library(container)
#> 
#> Attaching package: 'container'
#> The following object is masked from 'package:stats':
#> 
#>     update
#> The following object is masked from 'package:base':
#> 
#>     replace
co = container(1, b = NA, 1:3, c = container("a", 1))
co
#> [1, b = NA, (1L 2L 3L), c = ["a", 1]]
```

``` r
length(co)
#> [1] 4
```

``` r
names(co)
#> [1] ""  "b" ""  "c"
```

``` r
as.list(co)
#> [[1]]
#> [1] 1
#> 
#> $b
#> [1] NA
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> $c
#> ["a", 1]
```

``` r
unpack(co)
#>       b              c1  c2 
#> "1"  NA "1" "2" "3" "a" "1"
```
