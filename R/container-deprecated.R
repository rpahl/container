#' Deprecated Functions in Package container
#'
#' @description These functions are provided for backwards-compatibility and
#' may be defunct as soon as the next release.
#' @param ... (arguments)
#' @usage
#' empty(...)       ### -- use `is_empty(...)` instead
#' set(...)         ### -- use `setnew(...)` instead to create `Set`s
#' size(...)        ### -- use `length(...)` instead
#' sortkey(...)     ### -- `Dict` keys are now always sorted
#' remove(...)      ### -- use `delete(...)` instead
#' type(...)        ### -- not of use anymore
#' values(...)      ### -- use `as.list(...)` instead
#' @name container-deprecated
NULL

#' Check if object is empty
#'
#' @param x any `R` object.
#' @return `TRUE` if object is empty otherwise `FALSE`.
#' @name empty-deprecated
NULL

#' @rdname empty-deprecated
#' @export
empty <- function(x) UseMethod("empty")

#' @rdname empty-deprecated
#' @export
empty.Container <- function(x)
{
    .Deprecated("is_empty")
    is_empty(x)
}


#' @title Container size (deprecated)
#' @description This function is deprecated. Use [length()] instead.
#' @return `integer` size of the `Container`, that is, the number of
#' elements it contains.
#' @name size-deprecated
NULL

#' @rdname size-deprecated
#' @export
size <- function(x) UseMethod("size")

#' @rdname size-deprecated
#' @export
size.Container <- function(x)
{
    .Deprecated("length")
    length(x)
}


#' Sort Dict keys (deprecated)
#'
#' @description Re-order elements according to key-order. This function
#' is deprecated as keys are now always sorted.
#' @param x any `R` object.
#' @param decr `logical` Should the sort be increasing or decreasing?
#' @name sortkey-deprecated
NULL

#' @rdname sortkey-deprecated
#' @export
sortkey <- function(x, decr = FALSE) UseMethod("sortkey")

#' @rdname sortkey-deprecated
#' @return For `Dict` sorts the elements in place and inivisbly returns the
#' [Dict()] object.
#' @export
sortkey.Dict <- function(x, decr = FALSE) {
    .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
    invisible(x)
}


#' Get raw values of an object (deprecated)
#'
#' @param x any `R` object.
#' @return a copy of all elements as a basic `list`.
#' @name values-deprecated
NULL

#' @rdname values-deprecated
#' @export
values <- function(x)
    UseMethod("values")

#' @rdname values-deprecated
#' @export
values.Container <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}

#' @rdname values-deprecated
#' @export
values.dict.table <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}

