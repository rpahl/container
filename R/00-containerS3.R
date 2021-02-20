#' @title A sequence Container
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of the respective derived classes.
#' @param ... initial elements put into the `Container`.
#' @param elem some element of any type
#' @param x any `R` object for [as.container()] and [is.container()] and the
#' operators. An object of class `Container` for the `S3` methods.
#' @name ContainerS3
#' @seealso For the class documentation see [Container()] and it's derived
#' classes [Deque()], [Det()], and [Dict()].
NULL

#' @rdname ContainerS3
#' @details
#' Container methods:
#' * `container(...)` initializes and returns a [Container()] object.
#' @export
container <- function(...)
{
    Container$new(...)
}

#' @rdname ContainerS3
#' @details * [as.container(x)] coerces `x` to a container.
#' @export
as.container <- function(x)
{
    elems = if (length(x) == 0) list(x) else x
    do.call(container, args = as.list(elems))
}

#' @rdname ContainerS3
#' @details * `is.container(x)` returns `TRUE` if `x` is of class `Container`
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @rdname ContainerS3
#' @details * `as.list(x)` converts container `x` to a base `R` list.
#' @export
`as.list.Container` <- function(x) x$values()


#' @rdname ContainerS3
#' @details * `length(x)` returns the number of elements contained in `x`.
#' @export
length.Container <- function(x) x$length()

#' @rdname ContainerS3
#' @details * `names(x)` returns the names of the elements contained in `x`.
#' @export
names.Container <- function(x) names(x$values())


#' @rdname ContainerS3
#' @details * `unpack(x)` recursively unpacks any (possibly nested) recursive
#' structure into a flattened list.
#' @export
unpack = function(x) {

    .unpack = function(x) {
        if (is.container(x))
            rapply(as.list(x), f = .unpack)
        else
            unlist(x)
    }

    if (is.recursive(x))
        rapply(as.list(x), f = .unpack)
    else
        x
}

