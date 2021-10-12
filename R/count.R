#' Count Elements
#'
#' @description Count the number of occurences of some element.
#' @param x any `R` object.
#' @param elem element to counted.
#' @return `integer` number of how many times `elem` occurs in the object.
#' @export
count <- function(x, elem) UseMethod("count")

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


#' @name ContainerS3
#' @rdname ContainerS3
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


