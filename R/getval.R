#' Strict element access
#'
#' @description Try to access elements and signal an error if not found.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
getval <- function(x, ...) UseMethod("getval")

#' @rdname getval
#' @param key `character` name of key.
#' @return For `Dict` returns value at `key`, if `key` is found in `Dict`
#' otherwise throws an error.
#' @export
getval.Dict <- function(x, key) x$getval(key)

#' @rdname getval
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` returns the column if it does exist otherwise
#' throws an error.
#' @export
getval.dict.table <- function(x, column)
{
    if (has(x, column)) {
        peek(x, column)
    } else {
        stop("column '", column, "' not in ", data.class(x))
    }
}

