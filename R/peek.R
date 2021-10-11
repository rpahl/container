#' Peek at Left or Right of a Deque
#'
#' Try to access first or last element and return some default value if not found.
#' In contrast to `[at2()]`, this function provides a less stricter element
#' access, that is, it remains valid even if peeked elements don't exist.
#' @details
#' `peek` peek at last element of a `Deque`.
#'
#' `peekleft` peek at first element of a `Deque`.
#' @param x a `Deque` object.
#' @param default value to be returned if peeked value does not exist.
#' @return The first (`peekleft`) or last (`peek`) element.
#' @seealso [at2()] for strict element extraction
#' @name peek
#' @export
peekleft <- function(x, default = NULL) UseMethod("peekleft")

#' @rdname peek
#' @export
peek <- function(x, default = NULL) UseMethod("peek")


#' @rdname peek
#' @export
#' @examples
#' # Deque
#' d = deque(1, 2, 3)
#' peek(d)
#' peekleft(d)
#' peek(deque())
#' peek(deque(), default = 0)
#' peekleft(deque(), default = 0)
peek.Deque <- function(x, default = NULL) x$peek(default)

#' @rdname peek
#' @export
peekleft.Deque <- function(x, default = NULL) x$peekleft(default)


#' @name DequeS3
#' @rdname DequeS3
#' @details
#' * `peek(x, default = NULL)` peek at last element. If `x` is empty, return
#' `default`.
#' * `peekleft(x, default = NULL)` peek at first element. If `x` is empty,
#' return `default`.
#' @examples
#'
#' d = deque(1, 2, 3)
#' peek(d)
#' peekleft(d)
#' peek(deque())
#' peek(deque(), default = 0)
#' peekleft(deque(), default = 0)
NULL

