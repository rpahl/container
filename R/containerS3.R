#' @title A sequence Container
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of the respective derived classes.
#' @details For a full list of all container methods see [Container()].
#' @param ... initial elements put into the `Container`.
#' @param keep_names `logical` if TRUE, keeps names of passed elements.
#' @param x any `R` object, or an object inheriting from class 'Container' for
#' the S3 methods.
#' @return [container()] returns a [Container()] object.
#' @seealso [Container()], [deque()], [set()], [dict()]
#' @name ContainerS3
#' @export
container <- function(..., keep_names = FALSE) {
    if (missing(keep_names)) {
        Container$new(...)
    } else {
        Container$new(..., keep_names = keep_names)
    }
}

#' @rdname ContainerS3
#' @return [as.container()] coerces to a container.
#' @export
as.container <- function(x, ...)
{
    if (is.null(x)) return(container())
    UseMethod("as.container")
}

#' @export
as.container.default <- function(x, ...)
{
    if (is.container(x)) return(x)
    container(x, ...)
}

#' @rdname ContainerS3
#' @return [is.container()] returns `TRUE` if its argument is a [Container()]
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @rdname ContainerS3
#' @return `length()` returns the length of the internally stored sequence.
#' @export
length.Container <- function(x) x$length()

#' @rdname ContainerS3
#' @return `as.list()` returns a copy of the internally stored sequence as a
#' base R list.
#' @export
`as.list.Container` <- function(x) as.list(x$values())


