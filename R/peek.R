#' Peek element of an object
#'
#' Search an element in an object and return a copy of it. If there is no
#' element, return `NULL` or some default value.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
peek <- function(x, ...) UseMethod("peek")

#' @rdname peek
#' @return For `Deque` the last element at the right side of the deque.
#' @export
peek.Deque <- function(x) x$peek()

#' @rdname peek
#' @param key `character` key of the value to peek.
#' @param default `ANY` default value to return if `key` not in the dictionary.
#' @return For `Dict` if `key` does exists, the associated value is returned
#' otherwise the given `default` value.
#' @export
peek.Dict <- function(x, key, default = NULL) x$peek(key, default)



#' Peek element on left side of an object
#'
#' Search an element in an object from left to right and return a copy of the
#' first occurence.  If there is no element, return `NULL` or some default value.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
peekleft <- function(x) UseMethod("peekleft")

#' @rdname peekleft
#' @return For `Deque` the first element at the left side of the deque.
#' @export
peekleft.Deque <- function(x) x$peekleft()

