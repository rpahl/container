#' Sort elements according to their keys
#'
#' @description Re-order elements according to key-order.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param decreasing `logical` Should the sort be increasing or decreasing?
#' @export
sortkey <- function(x, decreasing = FALSE, ...) UseMethod("sortkey")

#' @rdname sortkey
#' @return For `Dict` sorts the elements in place and inivisbly returns the
#' [Dict()] object.
#' @export
sortkey.Dict <- function(x, decreasing = FALSE) x$sortkey(decreasing)

