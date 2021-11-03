
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

`container` provides an enhanced version of base R’s `list` with a
carefully designed set of extract, replace, and remove operations that
make it easier and safer to work with list-like data structures.

### Why `container`?

-   safe and flexible operations to
    -   extract (built-in default values, no unintended `NULL`)
    -   add and replace (mixed indices, no unintended overrides)
    -   remove (loose or strict deletion, remove by index or value)
-   compact printing
-   optional reference semantics

In addition, this package provides specialized data structures *Deque*,
*Set* and *Dict* and a *special* class `dict.table`, designed to extend
[data.table](https://CRAN.R-project.org/package=data.table) by container
operations to safely [Manage data columns with
dict.table](https://rpahl.github.io/container/articles/manage-data-columns.html).

### Installation

``` r
# Install release version from CRAN
install.packages("container")

# Install development version from GitHub
devtools::install_github("rpahl/container")
```

### Usage

``` r
library(container)
co <- container(colors = c("Red", "Green"), numbers = c(1, 2, 3), data = cars)
co
# [colors = ("Red" "Green"), numbers = (1 2 3), data = <<data.frame(50x2)>>]
```

Safe extract

``` r
at(co, "colours")   # oops
# Error: index 'colours' not found
at(co, "colors")
# [colors = ("Red" "Green")]
```

Safe remove

``` r
co <- delete_at(co, "colours")   # oops
# Error: names(s) not found: 'colours'
co <- delete_at(co, "colors")
co
# [numbers = (1 2 3), data = <<data.frame(50x2)>>]
```

Flexible peek

``` r
at(co, "colors")   # oops
# Error: index 'colors' not found
peek_at(co, "colors")
# []
peek_at(co, "colors", .default = c("black", "white"))
# [colors = ("black" "white")]
```

Safe replace

``` r
co <- replace_at(co, num = 1:10)   # oops
# Error: names(s) not found: 'num'
co <- replace_at(co, numbers = 1:10)
co
# [numbers = (1L 2L 3L 4L ...), data = <<data.frame(50x2)>>]
```

### Getting Started

-   [Introduction to
    container](https://rpahl.github.io/container/articles/container.html)
-   [Container operations for robust
    code](https://rpahl.github.io/container/articles/code-development.html)
-   [Manage parameter lists with
    dict](https://rpahl.github.io/container/articles/parameter-list.html)
-   [Manage data columns with
    dict.table](https://rpahl.github.io/container/articles/manage-data-columns.html)
