
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
release](https://www.r-pkg.org/badges/version/container)](https://cran.r-project.org/package=container)
[![Dependencies](https://tinyverse.netlify.app/badge/container)](https://CRAN.R-project.org/package=container)
[![Code
coverage](https://codecov.io/gh/rpahl/container/graph/badge.svg)](https://app.codecov.io/gh/rpahl/container)
[![CRAN
checks](https://badges.cranchecks.info/summary/container.svg)](https://cran.r-project.org/web/checks/check_results_container.html)
[![Downloads per
month](https://cranlogs.r-pkg.org/badges/last-month/container)](https://cran.r-project.org/package=container)
[![Downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/container)](https://cran.r-project.org/package=container)
[![Last
commit](https://img.shields.io/github/last-commit/rpahl/container.svg)](https://github.com/rpahl/container/commits)
[![Lifecycle
status](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

# container <img src="man/figures/logo.png" alt="logo" align="right" width="163" height="104"/>

The {container} package offers an enhanced version of base R’s `list`
with a carefully designed set of extract, replace, and remove operations
that make it easier and safer to work with list-like data structures.

### Why use {container}?

{container} objects work similar to base R lists and on top provide

- safe and flexible operations to
  - extract (custom default values, no unintended `NULL`)
  - add and replace (mixed indices, no unintended overrides)
  - remove (loose or strict deletion, remove by index or value)
- compact printing
- optional reference semantics

In addition, {container} provides specialized data structures [Deque,
Set, and Dict](articles/v05-deque-set-dict.html) and a *special* class
`dict.table`, designed to extend
[data.table](https://CRAN.R-project.org/package=data.table) by container
operations to safely [Manage data columns with
dict.table](articles/v04-manage-data-columns.html).

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

Use like a base R list

``` r
co[["colors"]] <- c("Blue", "Yellow")

co[["colors"]]
# [1] "Blue"   "Yellow"

co[2:1]
# [numbers = (1 2 3), colors = ("Blue" "Yellow")]
```

Safe extract

``` r
at(co, "colours")   # oops
# Error: index 'colours' not found

at(co, "colors")
# [colors = ("Blue" "Yellow")]
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

### Get started

- [Use container in interactive
  session](articles/v01-interactive-usage.html)
- [Use container for code
  development](articles/v02-code-development.html)
- [Manage parameter lists with dict](articles/v03-parameter-list.html)
- [Manage data columns with
  dict.table](articles/v04-manage-data-columns.html)

### When *not* to use {container}

Don’t bother using the {container} framework when *speed* is of high
importance. An exception is the `dict.table` class, which is very fast
as it is based on
[data.table](https://CRAN.R-project.org/package=data.table). Other than
that, if computation speed is critical for your application, we refer
you to using base R lists or packages that were optimized for
performance, such as the
[collections](https://CRAN.R-project.org/package=collections) or
[cppcontainers](https://cran.r-project.org/package=cppcontainers)
package.
