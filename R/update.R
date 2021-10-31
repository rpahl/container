#' Update Object with Elements from Another Object
#'
#' @description Takes an object and updates it with values from another object
#' by replacing the values at existing names and adding values at new names of
#' the other object. A common use case is to update parameter lists.
#' @param object any `R` object
#' @param other any object of the same type as `object`
#' @param ... additional arguments to be passed to or from methods.
#' @name update
#' @details `update` uses copy semantics while `ref_update` works by reference,
#' that is, updates in place.
NULL

#' @rdname update
#' @export
ref_update <- function(object, other, ...)
{
    if (!inherits(other, data.class(object))) {
        stop("'other' must be a ", data.class(object))
    }
    UseMethod("ref_update")
}

#' @rdname update
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' d1 = dict(a = 1, b = 2)
#' d2 = dict(       b = 0, c = 3)
#' update(d1, d2)  # {a = 1, b = 0, c = 3}
#' update(d2, d1)  # {a = 1, b = 2, c = 3}
#' @export
update.Container <- function(object, other, ...)
{
    if (!inherits(other, data.class(object))) {
        stop("'other' must be a ", data.class(object))
    }
    ref_update.Container(object$clone(deep = TRUE), other$clone(deep = TRUE))
}

#' @rdname update
#' @export
ref_update.Container <- function(object, other, ...)
{
    object$update(other)
}

#' @name DictS3
#' @rdname DictS3
#' @details
#' * `update(object, other)` and `ref_update(object, other)` adds elements of `other` dict
#' for keys not yet in `object` and replaces the values of existing keys.
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
update.dict.table <- function(object, other, ...)
{
    if (!inherits(other, data.class(object))) {
        stop("'other' must be a ", data.class(object))
    }
    (ref_update.dict.table(copy(object), copy(other)))
}


#' @rdname update
#' @export
ref_update.dict.table <- function(object, other, ...)
{
    if (!inherits(other, data.class(object)))
        stop("arg must be a ", data.class(object))

    for (key in colnames(other))
        ref_replace_at(object, key, other[[key]], .add = TRUE)

    object
}

#' @name dict.table
#' @rdname dict.table
#' @details
#' * `update(object, other)` and `ref_update(object, other)` adds columns of `other` dict
#' that are not yet in `object` and replaces the values at existing columns.
#' @examples
#'
#' # Update parts of tables (second overwrites columns of the first)
#' dit1 = dict.table(a = 1:2, b = 3:4)
#' dit2 = dict.table(         b = 5:6, c = 8:9)
#' update(dit1, dit2)
#' update(dit2, dit1)
NULL


#' @rdname update
#' @return For `list`, an updated object of class `list`.
#' @examples
#'
#' l1 = list(1, b = 2)
#' l2 = list(   b = 0, c = 3)
#' update(l1, l2)
#' update(l2, l1)
#' @export
update.list <- function(object, other, ...)
{
    if (!inherits(other, data.class(object))) {
        stop("'other' must be a ", data.class(object))
    }

    as.list(as.container(object)$update(as.container(other)))
}

