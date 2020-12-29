#' Set
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, elements are always unique in the [Container()] and
#' it provides typical set operations such as `union` and `intersect`.
#' @details For a full list of all set methods see [Set()].
#' @param ... initial elements put into the `Set`.
#' @param x any `R` object, or an object inheriting from class 'Set' or
#' 'Container' for the corresponding S3 methods.
#' @return [setnew()] returns a [Set()] object.
#' @seealso [Set()], [container()]
#' @name setS3
#' @export
setnew <- function(...) Set$new(...)


set <- function(...) {
    .Deprecated("setnew")
    setnew(...)
}

#' @rdname setS3
#' @return [as.set()] coerces to a set.
#' @export
as.set <- function(x, ...) {
    if (is.null(x)) return(setnew())
    UseMethod("as.set")
}

#' @rdname setS3
#' @export
as.set.Container <- function(x)
{
    setnew(values(x))
}

#' @export
as.set.default <- function(x)
{
    if (is.set(x)) return(x)
    setnew(x)
}

#' @rdname setS3
#' @return [is.set()] returns `TRUE` if its argument is a [Set()] and `FALSE`
#' otherwise.
#' @export
is.set <- function(x) inherits(x, "Set")


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


