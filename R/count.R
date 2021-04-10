#' Count elements
#'
#' @description Count the number of occurences of some element.
#' @param x any `R` object.
#' @param elem element to counted.
#' @param ... additional arguments to be passed to or from methods.
#' @return `integer` number of how many times `elem` occurs in the object.
#' @export
count <- function(x, elem, ...) UseMethod("count")

#' @rdname count
#' @export
#' @examples
#'
#' co = container("a", "b", "a", mean, mean)
#' count(co, "a")
#' count(co, mean)
#' count(co, "c")
count.Container <- function(x, elem) {
    x$count(elem)
}


#' @name clear.Container
#' @rdname ContainerS3
#' @usage
#' count(x)
#' @details
#' * `count(x, elem)` count how often `elem` occurs in `x`.
#' @examples
#'
#' co = container("a", "b", "a", mean, mean)
#' count(co, "a")
#' count(co, mean)
#' count(co, "c")
NULL


#' @rdname count
#' @export
count.Set <- function(x, elem) as.integer(x$has(elem))


