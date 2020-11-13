#' Set S3 interface
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, elements are always unique in the [Container()] and
#' it provides typical set operations such as `union` and `intersect`.
#' @details For a detailed documentation of all methods see [Set()].
#' @name setS3
#' @seealso [Container()], [`+.Set()`], [`/.Set()`], [`-.Set()`], [`<.Set()`],
#' [`>.Set()`]
NULL


#' @rdname setS3
#' @export
set <- function(x=list()) Set$new(x)

#' @rdname setS3
#' @export
as.set <- function(x) Set$new(x)

#' @rdname setS3
#' @export
is.set <- function(x) inherits(x, "Set")

#' @rdname setS3
#' @export
add.Set <- function(x, elem, ...) x$add(elem)


#' Binary set operators
#'
#' @description Binary operators for [Set()] objects.
#' @name setS3binOp
#' @param s1 [Set()] object
#' @param s2 [Set()] object
NULL

#' @rdname setS3binOp
#' @return For `!=` return `TRUE` if both sets are not equal.
#' @export
`!=.Set` <- function(s1, s2) !(s1$is.equal(s2))

#' @rdname setS3binOp
#' @return For `==` return `TRUE` if both sets are equal.
#' @export
`==.Set` <- function(s1, s2) s1$is.equal(s2)

#' @rdname setS3binOp
#' @return For `+` return union of both sets
#' @export
`+.Set` <- function(s1, s2) s1$union(s2)

#' @rdname setS3binOp
#' @return For `/` return intersection of both sets
#' @export
`/.Set` <- function(s1, s2) s1$intersect(s2)

#' @rdname setS3binOp
#' @return For `-` return set-difference of both sets
#' @export
`-.Set` <- function(s1, s2) s1$diff(s2)

#' @rdname setS3binOp
#' @return For `< return `TRUE` if s1 is subset of s2.
#' @export
`<.Set` <- function(s1, s2) s1$is.subset(s2)

#' @rdname setS3binOp
#' @return For `> return `TRUE` if s1 is superset of s2.
#' @export
`>.Set` <- function(s1, s2) s1$is.superset(s2)

