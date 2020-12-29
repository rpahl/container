#' Determine if object has some element
#'
#' @param x any `R` object.
#' @param elem element to search for.
#' @param ... additional arguments to be passed to or from methods.
#' @export
has <- function(x, ...) UseMethod("has")

#' @rdname has
#' @return For `Container` `TRUE` if element is in container otherwise `FALSE`.
#' @export
has.Container <- function(x, elem) x$has(elem)

#' @rdname has
#' @param key `character` name of key to search for.
#' @return For `Dict` `TRUE` if key is in dict otherwise `FALSE`.
#' @export
has.Dict <- function(x, key) x$has(key)

