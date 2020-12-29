#' Get raw values of an object
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return a copy of all elements if possible as `atomic` vector otherwise as
#' a basic `list`.
#' @export
values <- function(x, ...) UseMethod("values")

#' @rdname values
#' @export
values.Container <- function(x) x$values()

