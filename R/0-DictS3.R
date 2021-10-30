#' @title A Dictionary
#'
#' @description The [Dict] initially was developed to resemble Python's
#' dict type, but by now offers both more features and flexibility, for
#' example, by providing both associative key-value pair as well as
#' positional array semantics.
#' It is implemented as a specialized associative [Container] thus sharing
#' all [Container] methods with some of them being adapted to account for
#' the key-value pair semantic. All elements must be named.
#' @param ... elements put into the `Dict`.
#' @param x `R` object of `ANY` type for [as.dict()] and [is.dict()]
#' or of class `Dict` for the `S3` methods.
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Dict] and it's superclass [Container].
#' @name DictS3
#' @details Internally, all key-value pairs are stored in a hash-table and the
#' elements are sorted lexicographically by their keys.
#' Methods that alter `Dict` objects usually come in two versions
#' providing either copy or reference semantics where the latter start with
#' `'ref_'` to note the reference semantic, for example, `add()` and `ref_add()`.
#' @examples
#' d = dict(b = "one", a = 1, f = mean, na = NA)
#' print(d)
#' names(d)
#'
#' \dontrun{
#' dict(a = 1, 2)   # all elements must be named
#' }
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

methods::setOldClass("Dict")
methods::setAs("list", "Dict", function(from) as.dict(from))

#' @rdname DictS3
#' @details * `is.dict(x)` returns `TRUE` if `x` is of class `Dict`
#' and `FALSE` otherwise.
#' @export
is.dict <- function(x) inherits(x, "Dict")


#' @export
c.Dict <- function(..., recursive = FALSE)
{
    concat = c.Container(..., recursive = recursive, use.names = TRUE)

    if (recursive)
        concat
    else
        as.dict(concat)
}

