#' Add element
#'
#' Add an element to an object.
#' @param x an `R` object of the respective class.
#' @param elem some element of any type
#' @param ... additional arguments to be passed to or from methods.
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname add
#' @return For `Container`, an object of class `Container` with the value being
#' added to `x`.
#' @export
add.Container <- function(x, elem)
{
    if (length(elem) == 0)
        elem = list(elem)

    c(x, elem)
}

#' @name add.Container
#' @rdname ContainerS3
#' @usage ## S3 method for class 'Container'
#' add(x, elem)
#' @details * `add(x, elem)` adds `elem` to `x`.
NULL



#' @rdname add
#' @param key `character` unique key identifier
#' @param value some value
#' @return For `Dict`, an object of class `Dict` with the key-value pair being
#' added to `x`. If the `key` already existed, an error is given.
#' @export
add.Dict <- function(x, key, value)
{
    l = list()
    l[[key]] = value
    c(x, l)
}

#' @name add.Dict
#' @rdname DictS3
#' @usage ## S3 method for class 'Dict'
#' add(x, key, value)
#' @details * `add(x, key, value)` adds key-value pair to `x`. If the `key`
#' already existed, an error is given.
NULL



#' @rdname add
#' @param j `character` name or `numeric` index of column.
#' @param value some value
#' @return For `dict.table`, an object of class `dict.table` with the value
#' being added to `x` at column `j`. If the column already existed, an error is
#' given.
#' @export
add.dict.table <- function(x, j, value)
{
    if (has(x, j)) {
        stop("column '", j, "' already in ", data.class(x))
    }
    setval(x, j, value, add = TRUE)
}



#' @rdname add
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname add
#' @return For `Deque`, an object of class `Deque` with the element being
#' added to the left of `x`.
#' @export
addleft.Deque <- function(x, elem)
{
    if (length(elem) == 0)
        elem = list(elem)

    c(x, elem)$rotate(1L)
}

#' @name addleft.Deque
#' @rdname DequeS3
#' @usage ## S3 method for class 'Deque'
#' addleft(x, elem)
#' @details * `addleft(x, elem)` adds `elem` to left side of `x`.
NULL

