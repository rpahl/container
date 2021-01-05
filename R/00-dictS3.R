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
    do.call(dict, args = as.list(x))
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
keys <- function(x)
{
    if (!inherits(x, "Dict")) stop("x must be a 'Dict'")
    x$keys()
}


#' @rdname dictS3
#' @return `names()` returns the same as [keys()].
#' @export
names.Dict <- function(x) x$keys()


