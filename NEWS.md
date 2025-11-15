<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# container 1.1.0.9003

- Same as previous version.


# container 1.1.0.9002

- Same as previous version.


# container 1.1.0.9001

* docs: update interactive usage guide - cleanup old artifacts
* docs: update pkg site; include %in% operator in reference index


# container 1.1.0.9000

- Same as previous version.


# container 1.1.0

## New features

* Extraction (`[.Container`):
  * Non-standard evaluation (NSE) for ranges and mixed endpoints, e.g. `x[a:b]`,
    `x[1:c]`, `x[d:2]`, and negative ranges like `x[-(a:c)]`.
  * Multiple indices as separate arguments and `list(...)` sugar: `x[1, "a", 3:5]`,
    `x[list(1, "a")]`.
  * Logical indexing improvements with safe recycling and informative warnings when
    the mask length doesn’t divide the container length; `NA` treated as `FALSE` with a warning.
  * `.default` argument supported to fill unknown/out-of-bounds positive indices
    and unknown names while preserving order and duplicates.
  * `x[[TRUE]]` now aligns with base R to return the first element.

* Replacement (`[<-.Container`):
  * Supports the same extended indexing options as extraction, including NSE ranges,
    mixed indices, logical masks, and negative (complement) selection.
  * `x[] <- v` targets all positions; zero-length selections perform no replacement.
  * Adds unknown names when replacing via character indices; numeric indices must be in-bounds.
  * Replacement length is recycled with a warning when not a multiple of the target length.

* Operators & utilities:
  * Generic `%in%` operator added (#36).
  * Option for shallow copy when converting container to list.

## Fixes

* Allow setting names for initially unnamed containers.

## Internal / documentation

* Refactored extract indexing into internal helper `.get_pos_indices` and reused it
  for replace to ensure consistent semantics.
* Reworked and expanded tests; migrated legacy tests to testthat; improved docs and vignettes
  (interactive usage and replacement examples).
* Renamed vignettes for CRAN ordering (#33); updated README and vignettes (#30).
* Added News menu (this Changelog) to the package site and links to other packages via R universe.
* Clarified OpsReplace documentation (indexing options, NULL assignment, NSE alphanumeric ranges).


# container 1.0.5

* Update pages to bootstrap 5 and github action pipelines (#26)
  * upgrade pages to use bootstrap 5
  * update README
  * remove Date field in DESCRIPTION file
  * fix documentation signaled by CRAN devel checks

# container 1.0.4

* fix "don't run" examples
* fix Rd files regarding tidy HTML

# container 1.0.1

* prevent duplicated column names when renaming dict.tables
* don't check names in dict.table constructor - allow optional checking

# container 1.0.0

## Breaking changes

This update brings some big changes and breaks some of the earlier functions.
However, with version 1.0.0, from now on, the API will remain stable.

All classes `Container`, `Deque`, `Set` and `Dict` now have been modified to
be more similar in the way elements can be added, extracted, replaced or
removed and therefore share most of their operations. The motivation behind this
was to provide specialized data structures that enhance base R list but at
the same time still 'feel' familiar to R users. As a result, all classes
(including `Set` and `Dict`) now provide named elements and access via name or
integer indices similar to base R list, which should users enable to use any of
them right from the start without having to read through the manual.

A new class `dict.table` is provided, a mixture of
[data.table](https://CRAN.R-project.org/package=data.table) and `Dict`, which
extends `data.table` by `Dict` operations to enhance data column management.

List of changes

* `Set` and `Dict` provide positional access
* for `Dict`, internally all key-value pairs are stored in a hash-table and the
  elements are *always sorted* lexicographically by their keys
* in addition to `Set` there is a new class `OrderedSet`, in which all elements
  are also *always sorted*.
* objects of any class are now initialized with elements passed via
  `name = value` as is done in base R lists.
* `add()` now accepts multiple elements passed via `name = value`.
* `delete()` (formerly `remove`) and `discard()` now always work *by value*. For
  removing elements by index, now `delete_at()` and `discard_at()` are used.
* `has()` now always works *by value*. For checking elements by name, now
  `has_name()` is used.
* the `print()` method has been fully revised and now (inspired by the
  [sets](https://CRAN.R-project.org/package=sets) package) prints very compact.
* `size()` is now determined by `length()`
* `count()`, `pop`, and `update` are now available for all classes
* the S3 method interface now by default provides copy semantics. For reference
  semantics, functions starting with `ref_` are used.

## New features

* new classes `OrderedSet` and `dict.table`
* all classes support named elements
* `names()` is used to get and set the names as in base R list.
* extract or replace parts of an object via standard `[`, `[[`, and `$`
  operators using character or numeric indices or *both* at the same time.
* `container_options()` to customize printing and applied comparison operators
* `unpack()` to unpack nested container objects
* `replace()` and `replace_at()` to replace values and values at indices
* `at()` and `at2()` for safe element extraction
* `peek_at()` and `peek_at2()` for loose element peeking with default values
* `ref_pop()` get and remove element at given index
* `rename()` elements in place
* comparison operators that compare containers *lexicographically*
* arithmetic operators
* group generic functions for Math and Summary

## Deprecated Functions

* `empty()` - use `is_empty()` instead
* `set()` - use `setnew()` instead
* `size()` - use `length()` instead
* `sortkey()` - keys of `Dict` objects are now always sorted
* `remove()` - use `delete()` instead
* `type()` - not of use anymore
* `values()` - use `as.list()` instead

# container 0.3.0

Initial release
