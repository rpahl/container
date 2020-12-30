#' Count elements
#'
#' @description Count the number of occurences of some element.
#' @param x any `R` object.
#' @param elem element to counted.
#' @param ... additional arguments to be passed to or from methods.
#' @return Returns how many times `elem` occurs in the object.
#' @export
count <- function(x, elem, ...) UseMethod("count")

#' @rdname count
#' @export
count.Deque <- function(x, elem) x$count(elem)

#' @rdname count
#' @export
count.Set <- function(x, elem) as.integer(x$has(elem))

