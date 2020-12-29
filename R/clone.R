#' Clone an object
#'
#' Creates a copy of the object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @return A copy of the object.
#' @export
clone <- function(x, ...) UseMethod("clone")


#' @rdname clone
#' @param deep `logical` if `TRUE` a `deep` copy otherwise (default) a shallow
#' copy is performed.
#' @export
clone.Container <- function(x, deep = FALSE) x$clone(deep)

