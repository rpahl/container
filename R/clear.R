#' Clear a container
#'
#' Removes all elements from the container object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
clear <- function(x, ...) UseMethod("clear")

#' @rdname clear
#' @return For `Container`, an object of class `Container` with all elements
#' being removed.
#' @export
clear.Container <- function(x) x$clear()

#' @name clear.Container
#' @rdname ContainerS3
#' @usage ## S3 method for class 'Container'
#' clear(x)
#' @details * `clear(x)` removes all elements from `x`.
NULL

#' @rdname clear
#' @return For `dict.table`, an object of class `dict.table` with all elements
#' being removed.
#' @export
clear.dict.table <- function(x) delete(x, names(x))

#' @export
clear.default <- function(x) {
    stop("clear not implemented for '", data.class(x), "'")
}

