#' Clone an object
#'
#' Creates a copy of the object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return A copy of the object.
#' @export
clone <- function(x, ...) UseMethod("clone")


#' @rdname clone
#' @export
#' @examples
#'
#' co = container(1, 2, 3)
#' co2 = clone(co)
clone.Container <- function(x) x$clone(deep = TRUE)

#' @name clone.Container
#' @rdname ContainerS3
#' @usage
#' clone(x)
#' @details
#' * clone(x) create a copy of `x`.
#' @export
#' @examples
#'
#' co = container(1, 2, 3)
#' co2 = clone(co)
clone.Container <- function(x) x$clone(deep = TRUE)


#' @rdname clone
#' @export
#' @examples
#'
#' d = dict.table(a = 1:2, b = 3:4)
#' clone(d)
clone.dict.table <- function(x) copy(x)

#' @name clone.dict.table
#' @rdname dict.table
#' @usage
#' clone(x, ...)
#' @details
#' * clone(x) create a copy of `x`.
#' @export
#' @examples
#'
#' d = dict.table(a = 1:2, b = 3:4)
#' clone(d)
NULL

