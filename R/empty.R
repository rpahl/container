#' Check if object is empty
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return `TRUE` if object is empty otherwise `FALSE`.
#' @export
empty <- function(x, ...) UseMethod("empty")

#' @rdname ContainerS3
#' @details * `empty(x)` returns `TRUE` if `x` is empty and `FALSE` otherwise.
#' @export
empty.Container <- function(x) x$empty()

#' @rdname empty
#' @export
empty.dict.table <- function(x) ncol(x) == 0

