#' Dict methods
#'
#' @description The [dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [Container()] thus sharing all [container()]
#' methods with some of them being overridden to account for the associative
#' key-value pair semantic.
#' @details For a full list of all dict methods see [Dict()].
#' @param ... elements put into the `Dict`.
#' @param x `R` object of `ANY` type for [as.dict()] and [is.dict()]
#' or of class `Dict` for the `S3` methods.
#' @param key `character` name of key.
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Dict()] and it's superclass [Container()].
#' @name DictS3
#' @details Methods that alter `Dict` objects usually come in two versions
#' providing either copy or reference semantics where the latter are visible
#' by an underscore appended to the standard function name, for example,
#' `[delete()]` and `[delete_()]`.
#' ## Methods
#' @examples
#' d = dict(a = 1, b = "one", f = mean, na = NA)
#' print(d)
#' names(d)
#' as.list(d)
#' na.omit(d)
#' na.omit(as.list(d)) # does not work with lists
#'
#' d2 = dict(a = 2, b = list("one"))
#' d + d2  # same as update(d, d2)
#' d2 + d  # same as update(d2, d)
#' d - d2
#' d2 - d
#' d - d
#'
#' \dontrun{
#' c(d, d2)         # duplicated keys are not allowed for Dict
#' dict(a = 1, 2)   # all elements must be named}
NULL

#' @rdname DictS3
#' @details * `dict(...)` initializes and returns an object of class `Dict`
#' @export
dict <- function(...) Dict$new(...)$clone(deep = TRUE)

#' @examples
#'
#' # Coercion
#' as.dict(list(A = 1:3, B = "b"))
#' as.dict(c(x = 1, y = "x", z = 2 + 3))
#' @rdname DictS3
#' @details * `as.dict(x)` coerces `x` to a dictionary
#' @export
as.dict <- function(x)
{
    do.call(dict, args = as.list(x))
}

#' @rdname DictS3
#' @details * `is.dict(x)` returns `TRUE` if `x` is of class `Dict`
#' and `FALSE` otherwise.
#' @export
is.dict <- function(x) inherits(x, "Dict")


#' @export
keys <- function(x)
{
    .Deprecated("names")
    names(x)
}


#' @export
c.Dict <- function(..., recursive = FALSE)
{
    concat = c.Container(..., recursive = recursive, use.names = TRUE)

    if (recursive)
        concat
    else
        as.dict(concat)
}

