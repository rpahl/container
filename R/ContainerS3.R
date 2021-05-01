#' @title Container methods
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of these classes.
#' @param ... elements to put into or remove from the `Container`.
#' @param elem some element of any type
#' @param x `R` object of `ANY` type for [as.container()] and [is.container()]
#' or of class `Container` for the `S3` methods.
#' @name ContainerS3
#' @seealso For the `Container` class documentation see [Container()].
#' @details Methods that alter `Container` objects usually come in two versions
#' providing either copy or reference semantics where the latter are visible
#' by an underscore appended to the standard function name, for example,
#' `add` and `add_`.
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
#' @details * `is.container(x)` returns `TRUE` if `x` is of class `Container`
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @name as.list.Container
#' @rdname ContainerS3
#' @usage
#' ## S3 methods for class 'Container'
#' ## --------------------------------
#'
#' as.list(x)
#' @details * `as.list(x)` converts container `x` to a base `R` list. All of
#' the container's elements will copied (deeply) during the conversion.
NULL

#' @export
`as.list.Container` <- function(x) x$clone(deep = TRUE)$values()


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


#' @rdname ContainerS3
#' @details * `na.omit(x)` omits all `NA` values contained in `x`.
#' @export
na.omit.Container <- function(x)
{
    l = as.list(x)
    as.container(l[!is.na(l)])
}

