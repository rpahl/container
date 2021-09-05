#' Rotate elements
#'
#' @description Rotate all elements `n` steps to the right. If n is
#' negative, rotate to the left.
#' @param x any `R` object.
#' @param n `integer` number of steps to rotate
#' @param ... additional arguments to be passed to or from methods.
#' @details While `rotate` uses copy semantics, `ref_rotate` works by reference,
#' that is, rotates in place on the original object.
#' @export
rotate <- function(x, n = 1L) UseMethod("rotate")

#' @rdname rotate
#' @export
ref_rotate <- function(x, n = 1L) UseMethod("ref_rotate")


#' @rdname rotate
#' @return For `Deque` returns the rotated [Deque()] object.
#' @export
#' @examples
#' d = deque(1, 2, 3, 4)
#' rotate(d)
#' rotate(d, n = 2)
rotate.Deque <- function(x, n = 1L)
{
    (ref_rotate(x$clone(deep = TRUE), n))
}

#' @name rotate.Deque
#' @rdname DequeS3
#' @usage
#' rotate(x, n)
#' @details
#' * `rotate(x, n)` rotate all elements `n` steps to the right, If `n` is
#' negative, rotate to the left.
#' @examples
#'
#' d = deque(1, 2, 3, 4)
#' rotate(d)
#' rotate(d, n = 2)
#'
NULL

#' @rdname rotate
#' @export
ref_rotate.Deque <- function(x, n = 1L)
{
    invisible(x$rotate(n))
}

