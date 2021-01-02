#' Replace Operators for Containers
#'
#' @description Replace parts of `Container` or `dict.table` objects.
#' @name OpsReplace
#' @param x `Container` or `dict.table` object in which to replace elements or
#' @param key `character` name of key or column.
#' columns.
NULL


#' @rdname OpsReplace
#' @param add `logical` If `FALSE` and key is not yet in the dict, an
#'  error is signaled. This is different from standard R lists, where instead
#'  just a new entry would be generated. To behave like standard R lists, set
#'  `add = TRUE`, which also sets the value at the key, but will also add a
#'  new key-value pair if the key is not yet in the dictionary.
#' @param value A suitable replacement value.
#' @return For `Dict` `[[<-` replaces the value associated with `key`. If `key`
#' is not in the dict, by default, an error is raised, unless the `add`
#' argument was set to `TRUE` (see details at `add` description).
#' @export
`[[<-.Dict` <- function(x, key, add = FALSE, value)
{
    x$setval(key, value, add)
}


#' @rdname OpsReplace
#' @return For `Dict` `$` replaces the value associated with `key`. If `key`
#' is not in the dict, the value is added.
#' @export
`$<-.Dict` <- function(x, key, value)
{
    x$setval(key, value, add = TRUE)
}


#' @rdname OpsReplace
#' @return For `Dict`, `[<-` replaces the values at the given `keys`. If one or
#' more keys are not found, an error is signaled, unless `add` was set to
#' `TRUE`.
#' @export
`[<-.Dict` <- function(x, key, add = FALSE, value)
{
    if (length(key) != length(value)) {
        if (length(value) == 1) {
            value = rep_len(value, length.out = length(key))
        } else {
            stop("length of key and value must match unless length(value) == 1")
        }
    }

    # If the setval fails somewhere it would have changed the original dict
    # partly - therefore work on a clone.
    d = x$clone()
    set_value = function(key, value) d$setval(key, value, add)
    mapply(key, value, FUN = set_value)
    invisible(d)
}


#' @rdname OpsReplace
#' @param element an element of the [Set()]
#' @return For `Set`, `[[<-` replaces `element` by `value`. If the `element`
#' does not exist, an error is signaled, unless you pass `NULL`, in which case
#' the value is just added to the `Set`.
#' @export
`[[<-.Set` <- function(x, element, value)
{
    if (missing(element)) element <- NULL
    if (!has(x, element)) {
        if (length(element)) {
            stop("'", element, "' not found.")
        }
    } else {
        x$delete(element)
    }
    x$add(value)
    invisible(x)
}


#' @rdname OpsReplace
#' @param j `numeric` or `character` column index.
#' @return For `dict.table`, `[[` replaces the selected column. If the column
#' does not exist, an error is signaled, unless `add` was set to `TRUE`.
#' @export
`[[<-.dict.table` <- function(x, j, add = FALSE, value)
{
    # setval(x, j, value, add)
    # the above crashes due to memory issue so we need to work on a copy
    setval(copy(x), j, value, add)
}


#' @rdname OpsReplace
#' @return For `dict.table` `$` replaces the selected column. If the column
#' does not exist, it is added to the `dict.table`.
#' @export
`$<-.dict.table` <- function(x, key, value)
{
    if (has(x, key)) {
        setval(x, key, value)
    }
    else {
        setval(copy(x), key, value, add = TRUE)
    }
}

