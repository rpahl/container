#' @title Set methods
#'
#' @description The [Set()] is considered and implemented as a specialized
#' [Container()], that is, `Set` elements are always unique. It provides
#' typical set operations such as `union` and `intersect`.
#' @param ... initial elements put into the `Set`.
#' @param x `R` object of `ANY` type for [as.set()] and [is.set()]
#' or of class `Set` for the `S3` methods.
#' @name SetS3
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Set()] and it's superclass [Container()].
#' @details While the [Set()] class is based on the `R6` framework and
#' provides reference semantics, the methods described here provide an `S3`
#' interface with copy semantics. Note that any `S3` methods defined for the
#' `Container` class also work with `Set` objects.
#' ## Methods
#' @examples
#' s = setnew(1, b = NA, 1:3, c = container("a", 1))
#' is.set(s)
#' print(s)
#' length(s)
#' names(s)
#' as.list(s)
#' na.omit(s)
#' unpack(s)   # flatten recursively similar to unlist
#'
NULL

#' @rdname SetS3
#' @details * `setnew(...)` initializes and returns a [Set()] object.
#' @export
setnew <- function(...) Set$new(...)$clone(deep = TRUE)

set <- function(...) {
    .Deprecated("setnew")
    setnew(...)
}


#' @rdname SetS3
#' @details * `as.set(x)` coerces `x` to a set.
#' @export
as.set <- function(x) {
    if (length(x) == 0)
        return(setnew())

    do.call(setnew, args = as.list(x))
}


#' @rdname SetS3
#' @details * `is.set(x)` returns `TRUE` if `x` is of class `Set` and `FALSE`
#' otherwise.
#' @export
is.set <- function(x) inherits(x, "Set")


#' @export
c.Set <- function(..., recursive = FALSE, use.names = TRUE)
{
    concat = c.Container(..., recursive = recursive, use.names = use.names)

    if (recursive)
        concat
    else
        as.set(concat)
}

