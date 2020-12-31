#' Clear an object
#'
#' Removes all elements from the object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return The cleared object.
#' @export
clear <- function(x, ...) UseMethod("clear")

#' @rdname clear
#' @export
clear.Container <- function(x) x$clear()

#' @rdname clear
#' @export
clear.dict.table <- function(x) delete(x, names(x))

