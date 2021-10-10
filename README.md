
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

\[container\] extends the functionality of base R \[list\] and the
\[data.table\] package and with \[deque\], \[set\], and \[dict\]
provides common data structures not provided by base R, (resembling
Python’s dict type). In addition, it provides \[iterators\] and supports
both reference and copy semantics.

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

The \[container\] can be seen as a base R \[list\] with extended
functionality. There is basically nothing that you can do with a
\[list\] that can’t can also be done with the \[container\], but the
\[container\] is capable of much more. Below you find some (of many)
extra things the \[container\] is capable of.

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

the elements of a \[container\] object are printed in a very compact and
readable way.

``` r
co
#> [(1L 2L 3L 4L ...), l = list("a", 1)]
```

``` r
#str(co) # TODO
```

The \[container\] allows to find and replace elements directly without
the need to determine their index first, by passing them in curly
brackets. First, let’s see how to do it with the list.

``` r
index = which(sapply(li, identical, 1:10))
li[[index]] <- 1:3
li
#> [[1]]
#> [1] 1 2 3
#> 
#> $l
#> $l[[1]]
#> [1] "a"
#> 
#> $l[[2]]
#> [1] 1
```

``` r
co[[{1:10}]] <- 1:3
co
#> [(1L 2L 3L), l = list("a", 1)]
```

``` r
unpack(co)
#>              l1  l2 
#> "1" "2" "3" "a" "1"
```

### dict.table vs data.frame

The \[dict.table\] can be seen as a base R \[data.frame\] with extended
functionality. There is basically nothing that you can do with a
\[data.frame\] that can’t can also be done with the \[dict.table\], but
the \[dict.table\] is capable of much more. Below you find some (of
many) extra things the \[dict.table\] is capable of.

``` r
dit = dict.table(a = 1:2, b = 3:4)
dit
#> <dict.table> with 2 rows and 2 columns
#>    a b
#> 1: 1 3
#> 2: 2 4
```
