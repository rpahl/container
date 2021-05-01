#' Update elements
#'
#' @description Takes an object and updates it with values from another object
#' by replacing the values at existing keys and adding values for new keys of
#' the other object. For example, this can be used to update some parameter
#' lists.
#' @param x any `R` object
#' @param other any object of the same type as `x`
#' @param ... additional arguments to be passed to or from methods.
#' @details Note that `update` uses copy semantics while `update_` works by
#' reference, that is, updates in place.
#' @export
update <- function(x, other, ...)
{
    if (!inherits(other, data.class(x))) {
        stop("'other' must be a ", data.class(x))
    }
    UseMethod("update")
}

#' @rdname update
#' @export
update_ <- function(x, other, ...)
{
    if (!inherits(other, data.class(x))) {
        stop("'other' must be a ", data.class(x))
    }
    UseMethod("update_")
}

#' @rdname update
#' @return For `Dict`, an updated object of class `Dict`.
#' @examples
#'
#' d1 = dict(a = 1, b = 2)
#' d2 = dict(       b = 0, c = 3)
#' update(d1, d2)  # {a = 1, b = 0, c = 3}
#' update(d2, d1)  # {a = 1, b = 2, c = 3}
#' @export
update.Dict <- function(x, other)
{
    update_.Dict(x$clone(deep = TRUE), other$clone(deep = TRUE))
}

#' @rdname update
#' @export
update_.Dict <- function(x, other)
{
    x$update(other)
}

#' @name update.Dict
#' @param other another `Dict` object
#' @rdname DictS3
#' @usage
#' update(x, other)
#' update_(x, other)
#' @details
#' * `update(x, other)` and `update_(x, other)` adds elements of `other` dict
#' for keys not yet in `x` and replaces the values of existing keys.
#' @examples
#'
#' d1 = dict(a = 1, b = 2)
#' d2 = dict(       b = 0, c = 3)
#' update(d1, d2)  # {a = 1, b = 0, c = 3}
#' update(d2, d1)  # {a = 1, b = 2, c = 3}
NULL


#' @rdname update
#' @return For `dict.table` an object of class `dict.table`.
#' @examples
#'
#' dit1 = dict.table(a = 1:2, b = 3:4)
#' dit2 = dict.table(         b = 5:6, c = 8:9)
#' update(d1, d2)
#' update(d2, d1)
#' @export
update.dict.table <- function(x, other)
{
    (update_.dict.table(copy(x), copy(other)))
}


#' @rdname update
#' @export
update_.dict.table <- function(x, other)
{
    if (!inherits(other, data.class(x)))
        stop("arg must be a ", data.class(x))

    for (key in colnames(other))
        replace_(x, key, value = getval(other, key), add = TRUE)

    x
}

#' @name update.dict.table
#' @rdname dict.table
#' @param other another `dict.table` object
#' @usage
#' update(x, other)
#' update_(x, other)
#' @details
#' * `update(x, other)` and `update_(x, other)` adds columns of `other` dict
#' that are not yet in `x` and replaces the values at existing columns.
#' @examples
#' dit1 = dict.table(a = 1:2, b = 3:4)
#' dit2 = dict.table(         b = 5:6, c = 8:9)
#' update(d1, d2)
#' update(d2, d1)
NULL


#' @rdname update
#' @return For `list`, an updated object of class `list`.
#' @details When applied to `list`s, `update` adds elements of `other` for names
#' not yet in the list and replaces the values of existing names.
#' @examples
#'
#' l1 = list(1, b = 2)
#' l2 = list(   b = 0, c = 3)
#' update(l1, l2)
#' \dontrun{
#' update(l2, l1)  # all elements of 'other' must be named}
#' @export
update.list <- function(x, other)
{
    if (!all(nzchar(names(other)))) {
        stop("all elements of 'other' must be named")
    }
    x[names(other)] <- other
    x
}

