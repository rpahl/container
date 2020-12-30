#' Strict element access
#'
#' @description Try to access elements and signal an error if not found.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
getval <- function(x, ...) UseMethod("getval")

#' @rdname getval
#' @param key `character` name of key.
#' @return For `Dict` returns value at `key`, if `key` is found in `Dict`
#' otherwise throws an error.
#' @export
getval.Dict <- function(x, key) x$getval(key)

