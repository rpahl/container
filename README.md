
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/container)](https://cran.r-project.org/package=container)
[![codecov.io](https://codecov.io/github/rpahl/container/coverage.svg?branch=master)](https://codecov.io/github/rpahl/container?branch=master)
[![status](https://tinyverse.netlify.com/badge/container)](https://CRAN.R-project.org/package=container)
[![Downloads](http://cranlogs.r-pkg.org/badges/container)](http://www.r-pkg.org/pkg/container)
[![R build
status](https://github.com/rpahl/container/workflows/R-CMD-check/badge.svg)](https://github.com/rpahl/container/actions)
<!-- badges: end -->

# container <img src="man/figures/logo.png" align="right" width="163" height="104"/>

### Update to NEW version 1.0.0 soon on [CRAN](https://cran.r-project.org/).

### Why `container`?

`container` extends base R `list` functionality with the goal to serve
as an **easy** and **safe** to use `list` alternative. It is based on
`list` but provides

-   compact printing
-   feature rich add and extract operations (no unintended `NULL` or
    `NA`)
-   safe replace and removal operations (no unintended override or
    delete)
-   optional reference semantics
-   and some more …

to make life easier when writing **critical** R code and managing
**large** lists.

Based on the *Container* class, this package also comes with specialized
data structures *Deque*, *Set* and *Dict* and a *special* class
`dict.table`, which is designed to extend
[data.table](https://CRAN.R-project.org/package=data.table) by a rich
set of functions to manage data columns.

### Installation

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

### Usage

The `container` package was originally created to enhance code
development but since version 1.0.0 also gained some functionality that
can be useful in an interactive R session, where you most likely use a
`container` the same way you would use a base R `list`, plus some new
operations. For example,

``` r
library(container)
co <- container(x = cars[, 1], y = cars[, 2], data = cars)
co
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...), data = <<data.frame(50x2)>>]
```

some standard operations …

``` r
co[1:2]
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...)]
```

``` r
co[["x"]][1:10]
#  [1]  4  4  7  7  8  9 10 10 10 11
```

some new operations …

``` r
# Use any number of (mixed) indices
co[1:2, "data"]
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...), data = <<data.frame(50x2)>>]
```

``` r
# Replace element by value
co[[{cars}]] <- iris
co
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...), data = <<data.frame(150x5)>>]
```

``` r
co2 = container(x = 111, data = NULL, -111)
co2
# [x = 111, data = NULL, -111]
```

``` r
# Merge-update with other containers
co = update(co, co2)
co
# [x = 111, y = (2 10 4 22 ...), data = NULL, -111]
```

``` r
# Easier rename
rename(co, "x", "A")
# [A = 111, y = (2 10 4 22 ...), data = NULL, -111]
```

### Getting Started

There is much more to explore. To get started, see

-   the [Get started](articles/container.html) vignette
-   [Manage parameter lists with dict](articles/parameter-list.html)
-   [Manage data columns with
    dict.table](articles/manage-data-columns.html)
