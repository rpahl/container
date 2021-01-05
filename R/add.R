#' Add element
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname ContainerS3
#' @param elem some element of any type
#' @details * `add(x, elem)` adds `elem` to `x`.
#' @export
add.Container <- function(x, elem) x$add(elem)

#' @rdname add
#' @param key `character` unique key identifier
#' @param value some value
#' @return For `Dict`, the dict object with the key-element pair being added.
#' @export
add.Dict <- function(x, key, value) x$add(key, value)

#' @rdname add
#' @param column `character` name or `numeric` index of column.
#' @param value some value
#' @return For `dict.table`, the dict.table object with the column being added.
#' @export
add.dict.table <- function(x, column, value)
{
    if (has(x, column)) {
        stop("column '", column, "' already in ", data.class(x))
    }
    setval(x, column, value, add = TRUE)
}



#' Add element to the left
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname addleft
#' @param elem some element of any type
#' @return For `Deque`, the deque object with the element being added to the
#' left.
#' @export
addleft.Deque <- function(x, elem) x$addleft(elem)

