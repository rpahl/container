
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

## container vs list

The `container` can be seen as a basic R `list` with extended
functionality. There is basically nothing that you can do with a `list`
that can’t can be done with the `container`, but the `container` is
capable of much more. Below you find some (of many) extra things the
`container` is capable of.

``` r
library(container)
#> 
#> Attaching package: 'container'
#> The following object is masked from 'package:base':
#> 
#>     replace
co <- container(1:10, b = NA, l = list("a", 1))
li <- as.list(co)
```

Since they are similar, you can always transform back-and-forth between
both types using respectively `as.list` or `as.container`.

The `container` object by default prints more compact than the `list`.

``` r
co
#> [(1L 2L 3L 4L ...), b = NA, l = list("a", 1)]
li
#> [[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $b
#> [1] NA
#> 
#> $l
#> $l[[1]]
#> [1] "a"
#> 
#> $l[[2]]
#> [1] 1
```

``` r
str(co) # TODO
#> Classes 'Container', 'Iterable', 'R6' <Container>
#>   Inherits from: <Iterable>
#>   Public:
#>     add: function (value, name = NULL) 
#>     at: function (index) 
#>     at2: function (index) 
#>     clear: function () 
#>     clone: function (deep = FALSE) 
#>     count: function (elem) 
#>     delete: function (elem) 
#>     delete_at: function (index) 
#>     discard: function (elem) 
#>     discard_at: function (index) 
#>     empty: function () 
#>     get_compare_fun: function () 
#>     has: function (elem) 
#>     has_name: function (name) 
#>     initialize: function (...) 
#>     is_empty: function () 
#>     iter: function () 
#>     length: function () 
#>     peek_at: function (index, default = NULL) 
#>     peek_at2: function (index, default = NULL) 
#>     pop: function (index) 
#>     print: function (...) 
#>     remove: function (elem) 
#>     rename: function (old, new) 
#>     replace: function (old, new, add = FALSE) 
#>     replace_at: function (index, value, add = FALSE) 
#>     size: function () 
#>     type: function () 
#>     update: function (other) 
#>     values: function () 
#>   Private:
#>     .get_element_position: function (x, ...) 
#>     .get_index_position: function (index) 
#>     .rename: function (old, new) 
#>     .replace_value_at: function (pos, value, name) 
#>     .set_compare_fun: function (x) 
#>     .subset: function (x, ...) 
#>     .subset2: function (x, ...) 
#>     .verify_same_class: function (x) 
#>     compare_fun: function (target, current, ...) 
#>     compare_predicate: function (x) 
#>     create_iter: function () 
#>     deep_clone: function (name, value) 
#>     elems: list
```

The `container` allows to find and replace elements directly without the
need to determine their index first, by passing them in curly brackets.

``` r
index = which(sapply(li, identical, 1:10))
li[[index]] <- 1:5
li
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> $b
#> [1] NA
#> 
#> $l
#> $l[[1]]
#> [1] "a"
#> 
#> $l[[2]]
#> [1] 1
#co[[{1:10}]] <- 1:5 # TODO
#co[[{list("a", 1)}]] <- 1:5 # TODO
#co[c(T, F, T)] # TODO
co
#> [(1L 2L 3L 4L ...), b = NA, l = list("a", 1)]
```

``` r
unpack(co)
#>                                                      b   l1   l2 
#>  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" "10"   NA  "a"  "1"
```

## dict.table vs data.frame

The `dict.table` can be seen as a basic R `data.frame` with extended
functionality. There is basically nothing that you can do with a
`data.frame` that can’t can be done with the `dict.table`, but the
`dict.table` is capable of much more. Here are some examples for why you
might want to prefer the `dict.table`.

``` r
co = container(1, b = NA, 1:3, c = container("a", 1))
co
#> [1, b = NA, (1L 2L 3L), c = ["a", 1]]
```
