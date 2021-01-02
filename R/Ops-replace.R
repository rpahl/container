#' Replace Operators for Containers
#'
#' @description Replace parts of Container objects.
#' @name OpsReplace
#' @param x `Container` object in which to replace element(s)
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
#' @return For `Set`, `[[<-` replaces `element` by `value`.
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

