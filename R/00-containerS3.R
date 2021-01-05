#' @title A sequence Container
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of the respective derived classes.
#' @details
#' Methods inherited from [Iterable()]:
#' * `iter(x)` returns an [Iterator()] object to iterate over `x`. Note that
#' this works on a copy of `x`, that is, changing `x` after the iterator was
#' created will not be accessible by the iterator.
#' @param ... initial elements put into the `Container`.
#' @param keep_names `logical` if TRUE, keeps names of passed elements.
#' @param elem some element of any type
#' @param x any `R` object for [as.container()] and [is.container()]. An
#' object of class `Container` for the `S3` methods.
#' @name ContainerS3
#' @seealso For the class documentation see [Container()] and it's derived
#' classes [Deque()], [Det()], and [Dict()].
NULL


#' @rdname ContainerS3
#' @details
#' Container methods:
#' * `container(...)` initializes and returns a [Container()] object.
#' @export
container <- function(..., keep_names = FALSE) {
    if (missing(keep_names)) {
        Container$new(...)
    } else {
        Container$new(..., keep_names = keep_names)
    }
}

#' @rdname ContainerS3
#' @details * [as.container(x)] coerces `x` to a container.
#' @export
as.container <- function(x)
{
    if (length(x) == 0) return(container())
    UseMethod("as.container")
}

#' @export
as.container.default <- function(x)
{
    do.call(container, args = as.list(x))
}

#' @rdname ContainerS3
#' @details * `is.container(x)` returns `TRUE` if `x` is of class `Container`.
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @rdname ContainerS3
#' @details * `as.list(x)` converts container `x` to a base `R` list.
#' @export
`as.list.Container` <- function(x) x$values()


#' @rdname ContainerS3
#' @details * `length(x)` returns the number of elements in container `x`.
#' @export
length.Container <- function(x) x$length()

