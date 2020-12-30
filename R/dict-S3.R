#' Dictionary
#'
#' @description The [dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [container()] thus sharing all [container()]
#' methods with some of them being overridden to account for the associative
#' key-value pair semantic.
#' @details For a full list of all dict methods see [Dict()].
#' @param ... initial elements put into the `Dict`.
#' @param x any `R` object, or an object inheriting from class 'Dict' or
#' 'Container'.
#' @return [dict()] returns a [Dict()] object.
#' @seealso [Dict()], [container()]
#' @name dictS3
#' @export
dict <- function(...) {
    UseMethod("dict")
}

#' @rdname dictS3
#' @export
dict.data.frame <- function(...)
{
    Dict$new(as.list(...))
}

#' @export
dict.default <- function(...)
{
    Dict$new(...)
}

#' @rdname dictS3
#' @return [as.dict()] coerces to a dict.
#' @export
as.dict <- function(x)
{
    if (is.null(x)) return(dict())
    UseMethod("as.dict")
}

#' @rdname dictS3
#' @export
as.dict.data.frame <- function(x)
{
    dict(as.list(x))
}

#' @export
as.dict.default <- function(x, ...)
{
    if (is.dict(x)) return(x)
    dict(x, ...)
}

#' @rdname dictS3
#' @return [is.dict()] returns `TRUE` if its argument is a [Dict()] and `FALSE`
#' otherwise.
#' @export
is.dict <- function(x) inherits(x, "Dict")

#' @rdname dictS3
#' @return `keys()` returns a `character` vector of all the dict's keys.
#' @export
keys <- function(x) x$keys()

#' @rdname dictS3
#' @return `names()` returns the same as [keys()].
#' @export
names.Dict <- function(x) x$keys()



#' Arithmetic binary `dict` operators
#'
#' @description Arithmetic operators for [Dict()] objects.
#' @name dictS3binOp
#'
#' @param d1 [Dict()] object
#' @param d2 [Dict()] object
NULL

#' @rdname dictS3binOp
#' @return For `+` returns a copy of `d1` updated by `d2`.
#' @export
`+.Dict` <- function(d1, d2) d1$clone()$update(d2)

#' @rdname dictS3binOp
#' @return For `-` returns a copy of `d1` with all `d2` keys being discarded.
#' @export
`-.Dict` <- function(d1, d2)
{
    d1.clone <- d1$clone()
    lapply(d2$keys(), FUN = function(k) d1.clone$discard(k))
    d1.clone
}

#' @rdname dictS3binOp
#' @return For `/` returns a copy of `d1` containing only keys that exist in
#' both, that is, an intersection of the keys.
#' @export
`/.Dict` <- function(d1, d2)
{
    keys.both <- intersect(keys(d1), keys(d2))
    d <- dict()
    for (key in keys.both) {
        d$add(key, d1$getval(key))
    }
    d
}

#' Extract or replace parts of a `Dict`
#'
#' @description Access and assignment operators for [Dict()] objects.
#' @name dictS3replace
#'
#' @param x [Dict()] object.
#' @param key `character` name of elements to extract or replace.
NULL

#' @rdname dictS3replace
#' @param default A suitable default value.
#' @return For `[[` returns the element found at key, or `default` if not found.
#' @export
`[[.Dict` <- function(x, key, default = NULL)
{
    if (missing(default)) {
        x$getval(key)
    } else {
        x$peek(key, default)
    }
}


#' @rdname dictS3replace
#' @return For `[` returns a dictionary containing the extracted values.
#' @export
`[.Dict` <- function(x, key, default = NULL)
{
    d = dict()
    for (k in unique(key)) {
        value = if (missing(default)) {
            x$getval(k)
        } else {
            x$peek(k, default = default)
        }
        d$add(k, value)
    }
    d
}


#' @rdname dictS3replace
#'
#' @param add `logical` If `FALSE` and key is not yet in the dictionary, an
#'  error is signaled. This is different from standard R lists, where instead
#'  just a new entry would be generated. To behave like standard R lists, set
#'  `add = TRUE`, which also sets the value at the key, but will also add a
#'  new key-value pair if the key is not yet in the dictionary.
#' @param value A suitable replacement value.
#' @return For `[[<-` replaces the value associated with `key`. If `key` is not
#'  in the dictionary, by default, an error is raised, unless `add` was set to
#'  `TRUE`, in which case the value is added to the dictionary.
#' @export
`[[<-.Dict` <- function(x, key, add = FALSE, value)
{
    x$setval(key, value, add)
}


#' @rdname dictS3replace
#' @return For `[<-` replaces the values at the given `keys`.
#' @export
`[<-.Dict` <- function(x, key, add = FALSE, value)
{
    if (length(key) != length(value)) {
        stop("length of key and value must match")
    }

    set_value = function(key, value) x$setval(key, value, add)
    mapply(key, value, FUN = set_value)
    invisible(x)
}

