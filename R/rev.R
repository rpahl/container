#' Reverse Elements
#'
#' @description `rev` provides a reversed version of its argument.
#' @param x `Deque` object
#' @details `rev` uses copy semantics while `ref_rev` works by reference,
#' that is, it reverse all elements in place.
#' @seealso [base::rev()]
#' @name rev
NULL

#' @rdname rev
#' @export
ref_rev <- function(.x, ...) UseMethod("ref_rev")

#' @rdname rev
#' @export
ref_rev.Deque <- function(x) x$rev()


#' @rdname rev
#' @return For `Deque`, an object of class `Deque`
#' @examples
#'
#' d = deque(a = 1, b = 2, 3)
#' rev(d)
#' print(d)
#' ref_rev(d)
#' print(d)
#'
#' @export
rev.Deque <- function(x) x$clone(deep = TRUE)$rev()


#' @name DequeS3
#' @rdname DequeS3
#' @details
#' * `rev(x)` and `ref_rev(x)` reverses all elements being done on a copy or in
#' place, respectively.
#' @examples
#'
#' d = deque(a = 1, b = 2, 3)
#' rev(d)
#' print(d)
#' ref_rev(d)
#' print(d)
NULL
