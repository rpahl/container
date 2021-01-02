#' Extract Operators for Containers
#'
#' @description Extract parts of Container and dict.table objects.
#' @name OpsExtract
#' @param x `Container` or `dict.table` object from which to extract
#' elements or columns.
NULL

#' @rdname OpsExtract
#' @param key `character` vector with key(s).
#' @param default A suitable default value.
#' @return For `Dict`, `[[` returns the element found at key. If not found, an
#' error is signaled, unless `default` was specified, which then would be
#' returned instead.
#' @export
`[[.Dict` <- function(x, key, default = NULL)
{
    if (missing(default)) {
        x$getval(key)
    } else {
        x$peek(key, default)
    }
}

#' @rdname OpsExtract
#' @return For `Dict`, `[` returns a new dict containing the extracted elements
#' found at the keys. If one or more keys are not found, an error is signaled,
#' unless `default` was specified, which then would be put in place for all
#' missing keys.
#' @export
`[.Dict` <- function(x, key, default = NULL)
{
    d = dict()
    for (k in unique(key)) {
        value = if (missing(default)) {
            x$getval(k)
        } else {
            x$peek(k, default = default)
        }
        d$add(k, value)
    }
    d
}


#' @rdname OpsExtract
#' @param j `numeric` or `character` column index.
#' @return For `dict.table`, `[[` returns the selected column. If the
#' column does not exist, an error is signaled, unless `default` was specified,
#' which then would be returned instead.
#' @export
`[[.dict.table` <- function(x, j, default = NULL)
{
    if (missing(default)) {
        getval(x, j)
    } else {
        peek(x, j, default)
    }
}


#' @rdname OpsExtract
#' @return For `dict.table`, `$` returns the selected column. If the
#' column does not exist, an error is signaled.
#' @export
`$.dict.table` <- function(x, key)
{
    j = pmatch(key, names(x))
    if (is.na(j)) j = key

    getval(x, j)
}
