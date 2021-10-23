#' @title Set and ordered Set
#'
#' @description The [Set] is considered and implemented as a specialized
#' [Container], that is, `Set` elements are always unique. It provides
#' typical set operations such as `union` and `intersect`.
#' @param ... initial elements put into the `Set`.
#' @param .ordered `logical` if `TRUE` all elements in the [Set] will be
#' ordered.
#' @param x `R` object of `ANY` type for [as.set()] and [is.set()]
#' or of class `Set` for the `S3` methods.
#' @name SetS3
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Set] and it's superclass [Container].
#' @details
#' Methods that alter [Set] objects usually come in two versions
#' providing either copy or reference semantics where the latter start with
#' `'ref_'` to note the reference semantic, for example, [add()] and [ref_add()].
#' @examples
#' s = setnew(1, b = NA, 1:3, c = container("a", 1))
#' is.set(s)
#' print(s)
#' length(s)
#' names(s)
#' as.list(s)
#' unpack(s)   # flatten recursively similar to unlist
#'
#' so = setnew(2, 1, .ordered = TRUE)
#' print(so)
#' add(so, 0)
NULL

#' @rdname SetS3
#' @details * `setnew(...)` initializes and returns a [Set()] object.
#' @export
setnew <- function(..., .ordered = FALSE)
{
    if (.ordered)
        OrderedSet$new(...)$clone(deep = TRUE)
    else
        Set$new(...)$clone(deep = TRUE)
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
#' @details * `as.orderedset(x)` coerces `x` to an ordered set.
#' @export
as.orderedset <- function(x) {
    newset = function(...) setnew(..., .ordered = TRUE)
    if (length(x) == 0)
        return(newset())

    do.call(newset, args = as.list(x))
}

methods::setOldClass("Set")
methods::setAs("list", "Set", function(from) as.set(from))


#' @rdname SetS3
#' @details * `is.set(x)` returns `TRUE` if `x` is of class `Set` and `FALSE`
#' otherwise.
#' @export
is.set <- function(x) inherits(x, "Set")

#' @rdname SetS3
#' @details * `is.orderedset(x)` returns `TRUE` if `x` is of class `OrderedSet`
#' and `FALSE` otherwise.
#' @export
is.orderedset <- function(x) inherits(x, "OrderedSet")


#' @export
c.Set <- function(..., recursive = FALSE, use.names = TRUE)
{
    concat = c.Container(..., recursive = recursive, use.names = use.names)

    if (recursive)
        concat
    else
        as.set(concat)
}

#' @export
c.OrderedSet <- function(..., recursive = FALSE, use.names = TRUE)
{
    concat = c.Container(..., recursive = recursive, use.names = use.names)

    if (recursive)
        concat
    else {
        as.orderedset(concat)
    }
}

