#' @title Container - Enhancing R's list
#'
#' @description A container is a data structure with typical member
#' functions to insert, delete and access elements from the container
#' object. It can be considered as a base R [list] with
#' extended functionality. The [Container] class also serves as the base
#' class for [Deque], [Set], and [Dict] objects.
#' @param ... (possibly named) elements to be put into or removed from the
#' [Container], or additional arguments passed from and to methods.
#' @param x `R` object of `ANY` type for [as.container] and [is.container]
#' or of class [Container] for the `S3` methods.
#' @name ContainerS3
#' @seealso For the class documentation see [Container].
#' Objects of the derived classes can be created by [deque], [setnew], and
#' [dict].
#' @details
#' Methods that alter [Container] objects usually come in two versions
#' providing either copy or reference semantics where the latter start with
#' `'ref_'` to note the reference semantic, for example, [add()] and [ref_add()].
#' @examples
#' co = container(1:5, c = container("a", 1), l = list())
#' is.container(co)
#' print(co)
#' length(co)
#' names(co)
#'
#' unpack(co)   # flatten recursively similar to unlist
#'
NULL

#' @rdname ContainerS3
#' @details
#' * `container(...)` initializes and returns a [Container] object.
#' @export
container <- function(...)
{
    Container$new(...)$clone(deep = TRUE)
}

#' @rdname ContainerS3
#' @details
#' * `cont(...)` is a short cut for `container(...)`.
#' @export
cont <- function(...) container(...)


#' @rdname ContainerS3
#' @details * `as.container(x)` or `as.cont(x)` coerce `x` to a [Container]
#' @export
as.container <- function(x)
{
    do.call(container, args = as.list(x))
}

#' @rdname ContainerS3
#' @export
as.cont <- function(x) as.container(x)


#' @import methods
methods::setOldClass("Container")
methods::setAs("list", "Container", function(from) as.container(from))


#' @rdname ContainerS3
#' @details * `is.container(x)` check if `x` is a [Container]
#' @export
is.container <- function(x) inherits(x, "Container")


#' @rdname ContainerS3
#' @details * `as.list(x)` converts container `x` to a base R [list]. All of
#' the container's elements are copied (deeply) during the conversion.
#' @export
`as.list.Container` <- function(x, ...) x$clone(deep = TRUE)$values()


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
#' @details * `length(x)` return the number of elements contained in `x`.
#' @export
length.Container <- function(x) x$length()

#' @rdname ContainerS3
#' @details * `names(x)` return the names of the elements contained in `x`.
#' @export
names.Container <- function(x) names(x$values())


#' @export
str.Container <- function(object, ...)
{
    cat(data.class(object), "of", length(object), "\n")
    utils::str(as.list(object), no.list = TRUE, ...)
}


#' @export
"names<-.Container" <- function(x, value)
{
    x$rename(names(x), value)
}

# TODO: implement generic %in%

