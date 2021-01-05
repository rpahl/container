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
as.set <- function(x, ...)
{
    if (length(x) == 0) return(setnew())
    UseMethod("as.set")
}


#' @export
as.set.default <- function(x)
{
    do.call(setnew, args = as.list(x))
}


#' @rdname setS3
#' @return [is.set()] returns `TRUE` if its argument is a [Set()] and `FALSE`
#' otherwise.
#' @export
is.set <- function(x) inherits(x, "Set")


#' @rdname setS3
#' @param e some element of any type
#' @return `%e%` returns `TRUE` if e is an element of `x`
#' @export
`%e%` <- function(e, x)
{
    if (!is.set(x)) stop("x must be a 'Set'")
    x$has(e)
}

