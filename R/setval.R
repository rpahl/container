#' Strict element replacement
#'
#' @description Try to find and replace elements and signal an error if not
#' found, unless stated otherwise (see option `add`).
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param add `logical` if FALSE (default) and element was not found, an error is
#' given. Otherwise if `TRUE` the new element is not used as a replacement but
#' just added as a new element.
#' @export
setval <- function(x, ...) UseMethod("setval")

#' @rdname setval
#' @param key `character` name of key.
#' @return For `Dict` overrides `value` at `key` if `key` is already in the
#' `Dict`. If `key` not in `Dict`, an error is given unless `add` was set to
#' `TRUE` in which case the `value` is added under `key`.
#' Invisibly returns the altered [Dict()] object.
#' @export
setval.Dict <- function(x, key, value, add=FALSE) x$setval(key, value, add)

