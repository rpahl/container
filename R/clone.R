#' Clone an object
#'
#' Creates a copy of the object.
#' @param x any `R` object.
#' @return A copy of the object.
#' @export
clone <- function(x) UseMethod("clone")


#' @rdname clone
#' @export
#' @examples
#'
#' co = container(1, 2, 3)
#' co2 = clone(co)
#' co == co2
clone.Container <- function(x) (x$clone(deep = TRUE))

#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `clone(x)` create a copy of `x`.
#' @examples
#'
#' co = container(1, 2, 3)
#' co2 = clone(co)
#' co == co2
NULL


#' @rdname clone
#' @export
#' @examples
#'
#' d = dict.table(a = 1:2, b = 3:4)
#' d2 = clone(d)
#' ref_clear(d)
#' print(d2)
clone.dict.table <- function(x) (copy(x))

#' @name dict.table
#' @rdname dict.table
#' @details
#' * `clone(x)` create a copy of `x`.
#' @examples
#'
#' d = dict.table(a = 1:2, b = 3:4)
#' d2 = clone(d)
#' ref_clear(d)
#' print(d2)
NULL

