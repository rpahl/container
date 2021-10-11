#' Deprecated Functions
#'
#' @description These functions are provided for backwards-compatibility and
#' may be defunct as soon as the next release.
#' @param x any `R` object.
#' @details
#' * [empty()]      [is_empty()] instead
#' * [set()]        [setnew()] instead
#' * [size()]       use [length()] instead
#' * [sortkey()]    keys of [Dict] objects are now always sorted
#' * [remove()]     use [delete()] instead
#' * [type()]       not of use anymore
#' * [values()]     use [as.list()] instead
#' @name deprecated
NULL

#' @rdname deprecated
#' @export
empty <- function(x) UseMethod("empty")

#' @rdname deprecated
#' @export
empty.Container <- function(x)
{
    .Deprecated("is_empty")
    is_empty(x)
}


#' @rdname deprecated
#' @export
size <- function(x) UseMethod("size")

#' @rdname deprecated
#' @export
size.Container <- function(x)
{
    .Deprecated("length")
    length(x)
}

#' @rdname deprecated
#' @param decr `logical` sort decreasingly?
#' @export
sortkey <- function(x, decr = FALSE) UseMethod("sortkey")

#' @rdname deprecated
#' @export
sortkey.Dict <- function(x, decr = FALSE) {
    .Deprecated(msg = "'sort' is deprecated - keys are now always sorted")
    invisible(x)
}


#' @rdname deprecated
#' @export
values <- function(x)
    UseMethod("values")

#' @rdname deprecated
#' @export
values.Container <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}

#' @rdname deprecated
#' @export
values.dict.table <- function(x)
{
    .Deprecated("as.list")
    as.list(x)
}


#' @rdname deprecated
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


