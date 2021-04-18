#' Strict multiple element access
#'
#' @description Try to access elements and signal an error if any is not found.
#' This function is provided as a convenience function for interactive use to
#' enable easy access of multiple elements.
#' @param x any `R` object.
#' @param ... names or indices associated with the desired elements.
#' @seealso [getval()]
#' @export
getvalues <- function(x, ...) UseMethod("getvalues")


#' @rdname getvalues
#' @return A new `Dict` containing the desired the values.
#' @export
#' @examples
#'
getvalues.Dict <- function(x, ...)
{
    getval.Dict(x, unlist(list(...)))
}


#' @name getvalues.Container
#' @rdname ContainerS3
#' @usage
#' getvalues(key)
#' @details
#' * `getvalues(x, ...)` retrieve values at the given keys. If a key
#' does not exist, an error is given. For a single key the raw value associated
#' with the key is returned, otherwise a new `dict` object containing all
#' requested key-value pairs.
#' @examples
#'
NULL


#' @rdname getvalues
#' @export
#' @examples
#'
getvalues.dict.table <- function(x, ...)
{
    getval.dict.table(x, unlist(list(...)))
}


#' @name getvalues.dict.table
#' @rdname dict.table
#' @usage
#' getvalues(x, column)
#' @details
#' * `getvales(x, ...)` retrieve values at specified columns.
#' If a column does not exist, an error is given.
#' For a single column the raw column vector is
#' returned, otherwise a new `dict.table` object containing all requested columns.
#' @examples
#'
NULL


