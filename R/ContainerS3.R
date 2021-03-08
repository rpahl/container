#' @title Container methods
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of these classes.
#' @param ... initial elements put into the `Container`.
#' @param elem some element of any type
#' @param x `R` object of `ANY` type for [as.container()] and [is.container()]
#' or of class `Container` for the `S3` methods.
#' @name ContainerS3
#' @seealso For the `Container` class documentation see [Container()].
#' @details While the [Container()] class is based on the `R6` framework and
#' provides reference semantics, the methods described here provide an `S3`
#' interface with copy semantics.
#' ## Methods
NULL

#' @rdname ContainerS3
#' @details
#' * `container(...)` initializes and returns a [Container()] object.
#' @export
container <- function(...)
{
    Container$new(...)$clone(deep = TRUE)
}

#' @rdname ContainerS3
#' @details * `as.container(x)` coerces `x` to a container.
#' @export
as.container <- function(x)
{
    do.call(container, args = as.list(x))
}

#' @rdname ContainerS3
#' @details * `as.list(x)` converts container `x` to a base `R` list. All of
#' the container's elements will copied (deeply) during this conversion so that
#' the resulting list represents a true copy of the converted container.
#' @export
`as.list.Container` <- function(x) x$clone(deep = TRUE)$values()

#' @rdname ContainerS3
#' @details * `is.container(x)` returns `TRUE` if `x` is of class `Container`
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @export
c.Container <- function(..., recursive = FALSE, use.names = TRUE)
{
    elements = container(...)$values() # yields a deep copy of all elements

    if (recursive)
        return(unpack(elements, recursive = TRUE, use.names = use.names))

    to_list_if_container = function(x)
        if (is.container(x)) as.list(x) else x

    list_elements = lapply(elements, to_list_if_container)

    c.args = c(list_elements, list(use.names = use.names))
    concatenated_elements = do.call(c, args = c.args)
    as.container(concatenated_elements)
}


#' @rdname ContainerS3
#' @details * `length(x)` returns the number of elements contained in `x`.
#' @export
length.Container <- function(x) x$length()

#' @rdname ContainerS3
#' @details * `names(x)` returns the names of the elements contained in `x`.
#' @export
names.Container <- function(x) names(x$values())

# TODO: implement generic %in%

# TODO: implement na.omit

