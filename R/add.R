#' Add element
#'
#' @param x any `R` object.
#' @param elem some element to be added.
#' @param ... additional arguments to be passed to or from methods.
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname add
#' @return For `Container`, the container object with the element being added.
#' @export
add.Container <- function(x, elem) x$add(elem)

#' @rdname add
#' @param key `character` unique identifier of the item.
#' @param value value to be added.
#' @return For `Dict`, the dict object with the key-element pair being added.
#' @export
add.Dict <- function(x, key, value) x$add(key, value)

#' @rdname add
#' @param column `character` name or `numeric` index of column.
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
#' @param elem some element to be added.
#' @param ... additional arguments to be passed to or from methods.
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname addleft
#' @return For `Deque`, the deque object with the element being added to the
#' left.
#' @export
addleft.Deque <- function(x, elem) x$addleft(elem)

