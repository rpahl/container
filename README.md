
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

`container` extends base R `list` functionality with the goal to serve
as an **easy** and **safe** to use `list` alternative in **interactive**
and **serious** R code.

### Why `container` over `list`?

A `container` is based on `list` but provides

-   compact printing
-   feature rich add and extract operations (no unintended `NULL` or
    `NA`)
-   safe replace and removal operations (no unintended override or
    delete)
-   optional reference semantics
-   and some more …

which makes life specifically easier in **code development** and when
dealing with **large lists**.

In addition, this package provides specialized data structures *deque*,
*set*, *dict*, and *dict.table*, the latter to extend the
[data.table](https://CRAN.R-project.org/package=data.table) package.

### Installation

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

### Usage

In an interactive R session you most likely use a `container` the same
way you would use a base R `list`, plus some new operations.

``` r
library(container)
co <- container(x = cars[, 1], y = cars[, 2], data = cars)
co
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...), data = <<data.frame(50x2)>>]
```

Some standard operations …

``` r
co[1:2]
# [x = (4 4 7 7 ...), y = (2 10 4 22 ...)]
```

``` r
co[["x"]][1:10]
#  [1]  4  4  7  7  8  9 10 10 10 11
```

Some new operations …

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
-   [Manage parameter lists with dict](articles/01-parameter-list.html)
-   [Smartly select and mutate data columns with
    dict.table](articles/02-smart-select-and-mutate.html)
