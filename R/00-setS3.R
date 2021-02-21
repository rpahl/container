#' @title Set
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, `Set` elements are always unique. It provides
#' typical set operations such as `union` and `intersect`.
#' @param ... initial elements put into the `Set`.
#' @param x `R` object of `ANY` type for [as.set()] and [is.set()]
#' or of class `Set` for the `S3` methods.
#' @name setS3
#' @seealso [Set()], [container()]
NULL

#' @rdname setS3
#' @details `Set` S3 methods:
#' * `setnew(...)` initializes and returns a [Set()] object.
#' @export
setnew <- function(...) Set$new(...)

set <- function(...) {
    .Deprecated("setnew")
    setnew(...)
}


#' @rdname setS3
#' @details * `as.set(x)` coerces `x` to a set.
#' @export
as.set <- function(x)
{
    return(do.call(setnew, args = as.list(x)))
}


#' @rdname setS3
#' @details * `is.set(x)` returns `TRUE` if `x` is of class `Set` and `FALSE`
#' otherwise.
#' @export
is.set <- function(x) inherits(x, "Set")

