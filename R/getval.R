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
#' d = dict(a = 1, b = 1:3)
#' getval(d, "b")
#' getval(d, c("a", "b"))
#' getvalues(d, "a", "b")
getval.Dict <- function(x, key)
{
    if (length(key) == 1)
        return(x$get(key))

    d = dict()
    for (k in unique(key))
        d$add(k, x$get(k))

    d
}


#' @name getval.Dict
#' @rdname DictS3
#' @param key `character` single key string or vector of keys
#' @usage
#' getval(x, key)
#' @details
#' * `getval(x, key)` retrieves value at `key`, which can be specified as one ore
#' more character values. If a `key` does not exist, an error is given.
#' For a single key the raw value associated with
#' the key is returned, otherwise a new `dict` object containing all requested
#' key-value pairs.
#' @examples
#'
#' d = dict(a = 1, b = 1:3)
#' getval(d, "b")
#' getval(dit, c("a", "b")) # or
#' getvalues(d, "a", "b")
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
#' dit = dict.table(a = 1:3, b = 4:6)
#' getval(dit, "b")
#' getval(dit, c("a", "b")) # or
#' getvalues(dit, "a", "b")
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
#' @param key `character` or `numeric` vector of column names or indices
#' @usage
#' getval(x, key)
#' @details
#' * `getval(x, key)` get value(s) at column(s) specified via `key`.
#' If a column does not exist, an error is given.
#' For a single column the raw column vector is
#' returned, otherwise a new `dict.table` object containing all requested columns.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' getval(dit, "b")
#' getval(dit, c("a", "b"))
#' getvalues(dit, "a", "b")
NULL

