#' Strict element replacement
#'
#' @description Try to find and replace elements and signal an error if not
#' found, unless stated otherwise (see option `add`).
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param add `logical` if FALSE (default) and element was not found, an error is
#' given. Otherwise if `TRUE` the new element is not used as a replacement but
#' just added as a new element.
#' @export
setval <- function(x, ...) UseMethod("setval")

#' @rdname setval
#' @param key `character` name of key.
#' @return For `Dict` overrides `value` at `key` if `key` is already in the
#' `Dict`. If `key` not in `Dict`, an error is given unless `add` was set to
#' `TRUE` in which case the `value` is added under `key`.
#' Invisibly returns the altered [Dict()] object.
#' @export
setval.Dict <- function(x, key, value, add = FALSE)
{
    x$setval(key, value, add)
}

#' @rdname setval
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` overrides the values at the given column.
#' If `column` does not exist, an error is given unless `add` was set to
#' `TRUE` in which case the column is added if possible. All changes are made
#' by reference. Invisibly returns the altered [dict.table()] object.
#' @export
setval.dict.table <- function(x, column, value, add = FALSE)
{
    if (!add) {
        if (!has(x, column)) {
            stop("column '", column, "' not in ", data.class(x))
        }
    }
    j <- if (is.numeric(column)) as.integer(column) else column
    data.table::set(x, j = j, value = value)
}

