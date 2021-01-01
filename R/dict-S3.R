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
#' @examples
#' dict(a = 1, b = "one", f = mean)
#'
#' d1 = dict(a = 1, b = 2)
#' mode(values(d1))                     # 'list'
#' d2 = dict(c(a = 1, b = 2))
#' mode(values(d2))                     # 'numeric'
#'
#' \dontrun{
#' dict(a = 1, 2)                       # all elements must be named}
#'
#' # Initialize from data.frame
#' daf = data.frame(A = 1:3, B = 3:1)
#' d = dict(daf)
#' d
#' @export
dict <- function(...)
{
    Dict$new(...)
}

#' @rdname dictS3
#' @return [as.dict()] coerces to a dict.
#' @export
#' @examples
#'
#' # Coerce from other types
#' as.dict(list(A = 1:3, B = "b"))
#' as.dict(c(x = 1, y = "x", z = 2 + 3))
as.dict <- function(x, ...)
{
    if (is.null(x)) return(dict())
    UseMethod("as.dict")
}

#' @export
as.dict.default <- function(x)
{
    dict(x)
}

#' @rdname dictS3
#' @return [is.dict()] returns `TRUE` if its argument is a [Dict()] and `FALSE`
#' otherwise.
#' @export
is.dict <- function(x) inherits(x, "Dict")

#' @rdname dictS3
#' @return `keys()` returns a `character` vector of all the dict's keys.
#' @export
#' @examples
#'
#' d = dict(x = 1, y = 2)
#' keys(d)
#' names(d)
keys <- function(x) x$keys()

#' @rdname dictS3
#' @return `names()` returns the same as [keys()].
#' @export
names.Dict <- function(x) x$keys()



#' Extract or replace parts of a `Dict`
#'
#' @description Access and assignment operators for [Dict()] objects.
#' @name dictS3replace
#'
#' @param x [Dict()] object.
#' @param key `character` name of elements to extract or replace.
NULL



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

