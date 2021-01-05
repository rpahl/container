#' Clone an object
#'
#' Creates a copy of the object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return A copy of the object.
#' @export
clone <- function(x, ...) UseMethod("clone")


#' @rdname ContainerS3
#' @details * `clone(x)` returns a copy of `x`.
#' @export
clone.Container <- function(x) x$clone(deep = TRUE)

