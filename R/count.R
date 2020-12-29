#' Count elements
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return A copy of the object.
#' @return `integer` times the `elem` appears in the object.
#' @export
count <- function(x, ...) UseMethod("count")

#' @rdname count
#' @param elem element to counted.
#' @export
count.Deque <- function(x, elem) x$count(elem)

#' @rdname count
#' @param elem element to counted.
#' @export
count.Set <- function(x, elem) as.integer(x$has(elem))

