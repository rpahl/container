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
replace <- function(x, ...) UseMethod("replace")

#' @export
replace.Container <- function(x, old, new, add = FALSE)
{
}


#' @rdname replace
#' @param key `character` name of key.
#' @return For `Dict` overrides `value` at `key` if `key` is already in the
#' `Dict`. If `key` not in `Dict`, an error is given unless `add` was set to
#' `TRUE` in which case the `value` is added under `key`.
#' Invisibly returns the altered [Dict()] object.
#' @export
replace.Dict <- function(x, key, value, add = FALSE)
{
    x$replace(key, value, add)
}

#' @rdname replace
#' @param key `character` name or `numeric` index of column.
#' @return For `dict.table` overrides the values at the given column.
#' If `key` does not exist, an error is given unless `add` was set to
#' `TRUE` in which case the column is added if possible. All changes are made
#' by reference. Invisibly returns the altered [dict.table()] object.
#' @export
replace.dict.table <- function(x, key, value, add = FALSE)
{
    if (!add && !has(x, key)) {
        if (is.character(key))
            stop("column '", key, "' not in ", data.class(x), ". ",
                 "To add the column, use 'add = TRUE'.")
        else
            stop(key, " is outside range [1, ncol = ", ncol(x), "]")
    }

    j <- if (is.numeric(key)) as.integer(key) else key
    data.table::set(x, j = j, value = value)
}

