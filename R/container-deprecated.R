#' Deprecated Functions in Package container
#'
#' @description These functions are provided for backwards-compatibility and
#' may be defunct as soon as the next release.
#' @param x any `R` object.
#' @details
#' empty(...)       ### -- use `is_empty(...)` instead
#' set(...)         ### -- use `setnew(...)` instead to create `Set`s
#' size(...)        ### -- use `length(...)` instead
#' sortkey(...)     ### -- `Dict` keys are now always sorted
#' remove(...)      ### -- use `delete(...)` instead
#' type(...)        ### -- not of use anymore
#' values(...)      ### -- use `as.list(...)` instead
#' @name container-deprecated
NULL

#' @rdname container-deprecated
#' @export
empty <- function(x) UseMethod("empty")

#' @rdname container-deprecated
#' @export
empty.Container <- function(x)
{
    .Deprecated("is_empty")
    is_empty(x)
}


#' @rdname container-deprecated
#' @export
size <- function(x) UseMethod("size")

#' @rdname container-deprecated
#' @export
size.Container <- function(x)
{
    .Deprecated("length")
    length(x)
}

#' @rdname container-deprecated
#' @param decr `logical` sort decreasingly?
#' @export
sortkey <- function(x, decr = FALSE) UseMethod("sortkey")

#' @rdname container-deprecated
#' @export
sortkey.Dict <- function(x, decr = FALSE) {
    .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
    invisible(x)
}


#' @rdname container-deprecated
#' @export
values <- function(x)
    UseMethod("values")

#' @rdname container-deprecated
#' @export
values.Container <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}

#' @rdname container-deprecated
#' @export
values.dict.table <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}


#' @rdname container-deprecated
#' @export
keys <- function(x)
{
    .Deprecated("names")
    names(x)
}


set <- function(...) {
    .Deprecated("setnew")
    setnew(...)
}


