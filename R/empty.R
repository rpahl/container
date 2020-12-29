#' Check if object is empty
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return `TRUE` if object is empty otherwise `FALSE`.
#' @export
empty <- function(x, ...) UseMethod("empty")

#' @rdname empty
#' @export
empty.Container <- function(x) x$empty()

