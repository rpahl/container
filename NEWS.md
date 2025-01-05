# container (development version)

* update README and vignettes
* add News menu including this Changelog to package site
* link to other packges via [my R universe](https://rpahl.r-universe.dev/packages)

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
