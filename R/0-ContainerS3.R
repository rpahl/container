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
#' `'ref_'` to note the reference semantic, for example, [add()] and
#' [ref_add()].
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
#' @param deep `logical(1)` indicating whether a deep copy of the elements
#' should be made when converting to a list.
#' @details * `as.list(x)` converts container `x` to a base R [list]. If
#' `deep` is `TRUE`, all of the container's elements are copied (deeply)
#' during the conversion.
#' @export
`as.list.Container` <- function(x, deep = TRUE, ...) {
    x$clone(deep = deep)$values()
}


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


#' @rdname ContainerS3
#' @param value `character` vector of names.
#' @details * `names(x) <- value` sets the names of `x`.
#' @export
"names<-.Container" <- function(x, value)
{
    # Enable renaming of unnamed containers or unnaming container
    isUnnamed <- is.null(names(x)) || all(names(x) == "")
    hasCompleteNewNames <- !is.null(value) && length(value) == length(x)
    if ((isUnnamed && hasCompleteNewNames) || is.null(value)) {
        l <- as.list(x)
        names(l) <- value
        return(as.container(l))
    }

    x$rename(as.character(names(x)), value)
}


#' @title S4 methods for Container
#'
#' @rdname ContainerS4
#' @name ContainerS4
#' @docType methods
NULL

#' @rdname ContainerS4
#' @description Membership operator for Container
#' @return A `logical` vector indicating if each element of `x` is found
#'
#' @examples
#' co <- container(a = 1, 2, b = 3)
#' c(1, 3, 5) %in% co                       # TRUE TRUE FALSE
#' c(x = 1, y = 2, z = 9) %in% co           # c(x = TRUE, y = TRUE, z = FALSE)
#' list(x = 1, y = 2, z = 9) %in% co        # c(x = TRUE, y = TRUE, z = FALSE)
#'
#' co2 <- container(a = NA, b = 2)
#' v <- c(x = 1, y = NA_real_, z = 2)
#' v %in% co2                               # c(x = FALSE, y = FALSE, z = TRUE)
#' co2 %in% v                               # c(a = FALSE, b = TRUE)
#' @export
methods::setGeneric("%in%", function(x, table) standardGeneric("%in%"),
    useAsDefault = function(x, table) base::`%in%`(x, table)
)

.eq_deep_ <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))

.any_match_top_ <- function(el, table_list) {
    any(vapply(table_list, .eq_deep_, b = el, FUN.VALUE = logical(1)))
}

#' @rdname ContainerS4
#' @aliases ANY,Container
#' @param x `ANY` value
#' @param table ``Container` object
#' @export
methods::setMethod("%in%",
    signature(x = "ANY", table = "Container"),
    function(x, table) {
        vals <- as.list(table)                # list of top-level values
        xl <- if (is.list(x)) x else as.list(x)
        vapply(xl, .any_match_top_, table_list = vals, FUN.VALUE = logical(1))
    }
)

#' @rdname ContainerS4
#' @aliases Container,ANY
#' @param x `Container` objects
#' @param table `ANY` value
#' @export
methods::setMethod("%in%", signature(x = "Container", table = "ANY"),
    function(x, table) {
        left <- as.list(x)
        right <- if (is.list(table)) table else as.list(table)
        vapply(left, .any_match_top_, table_list = right, FUN.VALUE = logical(1))
    }
)

#' @rdname ContainerS4
#' @aliases Container,Container
#' @param x `Container` objects
#' @param table `Container` objects
#' @export
methods::setMethod("%in%", signature(x = "Container", table = "Container"),
    function(x, table) {
        left <- as.list(x)
        right <- as.list(table)
        vapply(left, .any_match_top_, table_list = right, FUN.VALUE = logical(1))
    }
)
