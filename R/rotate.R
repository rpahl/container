#' Rotate elements
#'
#' @description Rotate all elements `n` steps to the right. If n is
#' negative, rotate to the left.
#' @param x any `R` object.
#' @param n `integer` number of steps to rotate
#' @param ... additional arguments to be passed to or from methods.
#' @export
rotate <- function(x, ...) UseMethod("rotate")

#' @rdname rotate
#' @return For `Deque` invisibly returns the [Deque()] object rotated in place.
#' @export
rotate.Deque <- function(x, n = 1L) x$rotate(n)

