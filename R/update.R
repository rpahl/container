#' Update elements
#'
#' @description Takes an object and updates it with values from another object
#' by replacing the values at existing keys and adding values for new keys of
#' the other object. For example, this can be used to update some parameter
#' lists.
#' @param x any `R` object
#' @param other any object of the same type as `x`
#' @param ... additional arguments to be passed to or from methods.
#' @export
update <- function(x, other, ...)
{
    if (!inherits(other, data.class(x))) {
        stop("'other' must be a ", data.class(x))
    }
    UseMethod("update")
}

#' @rdname update
#' @return For `Dict` adds elements of `other` dict for keys not yet in
#' the dict and replaces the values of existing keys otherwise.
#' @export
update.Dict <- function(x, other) x$update(other)

#' @rdname update
#' @return For `list` adds elements of `other` for names not yet in the
#' list and replaces the values of existing names otherwise.
#' @export
update.list <- function(x, other)
{
    if (!all(nzchar(names(other)))) {
        stop("all elements of 'other' must be named")
    }
    x[names(other)] <- other
    x
}

