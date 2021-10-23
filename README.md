
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

### Update to NEW version 1.0.0 soon on [CRAN](https://cran.r-project.org/).

*container* extends base R *list* functionality with the goal to serve
as an *easy and safe* to use *list* alternative, applicable not only for
interactive R sessions but specifically to make life easier in *serious
code* development.

In addition, this package provides specialized data structures *deque*,
*set*, *dict*, and *dict.table*, the latter to extend the
[data.table](https://CRAN.R-project.org/package=data.table) package.

### Why `container` over list?

A `container` is based on `list` but with

-   compact printing
-   feature rich add, extract, replace, or removal operations
-   safer data operations (no more unintended `NULL` or `NA`)
-   optional reference semantics
-   some more …

### Installation

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

### Getting Started
