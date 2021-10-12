
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/container)](https://cran.r-project.org/package=container)
[![codecov.io](https://codecov.io/github/rpahl/container/coverage.svg?branch=master)](https://codecov.io/github/rpahl/container?branch=master)
[![status](https://tinyverse.netlify.com/badge/container)](https://CRAN.R-project.org/package=container)
[![Downloads](http://cranlogs.r-pkg.org/badges/container)](http://www.r-pkg.org/pkg/container)
[![R build
status](https://github.com/rpahl/container/workflows/R-CMD-check/badge.svg)](https://github.com/rpahl/container/actions)
<!-- badges: end -->

# container <img src="images/logo.png" align="right" width="163" height="104"/>

## Update

Update to version 1.0.0 is coming soon to
[CRAN](https://cran.r-project.org/).

## Intro

This package extends the functionality of base R \[list\] and the
\[data.table\] package and with \[deque\], \[set\], and \[dict\]
provides additional common data structures. It furthermore implements
\[iterators\] and supports both reference and copy semantics.

## Why `container`?

## Features

## Installation

You can install the released version of container from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

### container vs list

Basically all you can do with a \[list\] can also be done with a
\[container\], but the \[container\] is capable of much more.

``` r
library(container)

co <- container(1:10, l = list("a", 1))
li <- as.list(co)
```

While the \[list\] output can be very long and hard to read

``` r
li
#> [[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $l
#> $l[[1]]
#> [1] "a"
#> 
#> $l[[2]]
#> [1] 1
```

the elements of a \[container\] object are printed very compact.

``` r
co
#> [(1L 2L 3L 4L ...), l = list("a", 1)]
```

Find and replace of \[list\] elements requires to determine the index
first.

``` r
element = list("a", 1)
index = which(sapply(li, identical, list("a", 1)))
li[[index]] <- 1:3
li
#> [[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $l
#> [1] 1 2 3
```

With \[container\] just pass the element directly in `{}`

``` r
co[[{element}]] <- 1:3
co
#> [(1L 2L 3L 4L ...), l = (1L 2L 3L)]
```

Update parameter lists with ease.

``` r
param = cont(x = 1, foo = "bar") # cont is a shortcut for container
param
#> [x = 1, foo = "bar"]
```

``` r
new_param = cont(z = 2, foo = "my foo")
update(param, new_param)
#> [x = 1, foo = "my foo", z = 2]
```

### dict.table vs data.frame

Basically all you can do with a \[data.table\] can also be done with a
\[dict.table\], but the \[dict.table\] also provides \[dict\]
functionality.

``` r
dit = dict.table(a = 1:2, b = 3:4)
dit
#> <dict.table> with 2 rows and 2 columns
#>    a b
#> 1: 1 3
#> 2: 2 4
```
