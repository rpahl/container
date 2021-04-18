#' Strict element access
#'
#' @description Try to access element and signal an error if not found.
#' @param x any `R` object.
#' @param key one or more indices to be accessed. For `Dict` objects the keys
#' must be of type `character` while they can be `character` or `numeric` for
#' `dict.table` objects.
#' @return If the `key` is of length one, the raw value associated with the
#' `key` is returned, otherwise an object of the same type as the one that was
#' accessed.
#' @param ... additional arguments to be passed to or from methods.
#' @export
getval <- function(x, ...) UseMethod("getval")


#' @rdname getval
#' @export
#' @examples
#'
getval.Dict <- function(x, key)
{
    if (length(key) == 1)
        return(x$get(key))

    d = dict()
    for (k in unique(key))
        d$add(k, x$get(k))

    d
}


#' @name getval.Container
#' @rdname ContainerS3
#' @usage
#' getval(x, key)
#' @details
#' * `getval(x, key)` retrieves value at `key`. If a `key`s does
#' not exist, an error is given. For a single key the raw value associated with
#' the key is returned, otherwise a new `dict` object containing all requested
#' key-value pairs.
#' @examples
#'
NULL


.get_dict.table_value <- function(x, key)
{
    if (!has(x, key))
        stop("column '", key, "' not in ", data.class(x))

    peek(x, key)
}


#' @rdname getval
#' @export
#' @examples
#'
getval.dict.table <- function(x, key)
{
    if (length(key) == 1)
        return(.get_dict.table_value(x, key))

    d = dict.table()
    for (k in unique(key)) {
        val = getval(x, k)
        if (is.numeric(k))
            k = colnames(x)[k]

        replace_.dict.table(d, k, val, add = TRUE)
    }

    d
}


#' @name getval.dict.table
#' @rdname dict.table
#' @usage
#' getval(x, key)
#' @details
#' * `getval(x, key)` get value at column(s) specified via `key`.
#' If a column does not exist, an error is given.
#' For a single column the raw column vector is
#' returned, otherwise a new `dict.table` object containing all requested columns.
#' @examples
#'
NULL


