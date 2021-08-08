#' @title Container methods
#'
#' @description A [Container()] is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. The [Container()] class serves as the base class for [Deque()],
#' '[Set()], and [Dict()] objects, which can be created by calls to
#' '[deque()], [setnew()], and [dict()], respectively.
#' @param ... (possibly named) elements to be put into or removed from the `Container`.
#' @param elem some element of any type
#' @param x `R` object of `ANY` type for [as.container()] and [is.container()]
#' or of class `Container` for the `S3` methods.
#' @param .x `object of class `Container`
#' @name ContainerS3
#' @seealso For the `Container` class documentation see [Container()]. To
#' create objects of the derived classes see [deque()], [setnew()], and
#' [dict()].
#' @details Methods that alter `Container` objects usually come in two versions
#' providing either copy or reference semantics where the latter are visible
#' by an underscore appended to the standard function name, for example,
#' `[add()]` and `[add_()]`.
#' ## Methods
#' @examples
#' co = container(1, b = NA, 1:3, c = container("a", 1))
#' is.container(co)
#' print(co)
#' length(co)
#' names(co)
#' as.list(co)
#' na.omit(co)
#' unpack(co)   # flatten recursively similar to unlist
#'
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

methods::setOldClass("Container")
methods::setAs("list", "Container", function(from) as.container(from))


#' @rdname ContainerS3
#' @details * `is.container(x)` returns `TRUE` if `x` is of class `Container`
#' and `FALSE` otherwise.
#' @export
is.container <- function(x) inherits(x, "Container")


#' @rdname ContainerS3
#' @details * `as.list(x)` converts container `x` to a base `R` list. All of
#' the container's elements will copied (deeply) during the conversion.
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


#' @rdname ContainerS3
#' @details * `na.omit(x)` omits all `NA` values contained in `x`.
#' @export
na.omit.Container <- function(x)
{
    l = as.list(x)
    as.container(l[!is.na(l)])
}


# TODO: implement generic %in%
