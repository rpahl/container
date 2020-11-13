#' Dict S3 interface
#'
#' @description The [dict()] resembles Python's dict type, and is implemented
#' as a specialized associative [container()] thus sharing all [container()]
#' methods with some of them being overridden to account for the associative
#' key-value pair semantic.
#' @details For a detailed documentation of all methods see [Dict()].
#' @name dictS3
#' @seealso [Container()], [+.Dict()], [-.Dict()], [[[<-.Dict()], [[[.Dict()],
#' [[<-.Dict()], [[.Dict()]
NULL

# S3 generic methods not derived from container

#' @export
getval <- function(x, ...) UseMethod("getval")

#' @export
keys <- function(x) UseMethod("keys")

#' @export
peek <- function(x, ...) UseMethod("peek")

#' @export
pop <- function(x, ...) UseMethod("pop")

#' @export
popitem <- function(x) UseMethod("popitem")

#' @export
setval <- function(x, ...) UseMethod("setval")

#' @export
sortkey <- function(x, ...) UseMethod("sortkey")

#' @rdname dictS3
#' @export
dict <- function(x = list()) Dict$new(x)

#' @rdname dictS3
#' @export
as.dict <- function(x) dict(x)

#' @rdname dictS3
#' @export
is.dict <- function(x) inherits(x, "Dict")

#' @rdname dictS3
#' @export
add.Dict <- function(x, key, value, ...) x$add(key, value)

#' @rdname dictS3
#' @export
discard.Dict <- function(x, key, ...) x$discard(key)

#' @rdname dictS3
#' @export
getval.Dict <- function(x, key, ...) x$get(key)

#' @rdname dictS3
#' @export
has.Dict <- function(x, key, ...) x$has(key)

#' @rdname dictS3
#' @export
keys.Dict <- function(x) x$keys()

#' @rdname dictS3
#' @export
peek.Dict <- function(x, key, default=NULL, ...) x$peek(key, default)

#' @rdname dictS3
#' @export
pop.Dict <- function(x, key, ...) x$pop(key)

#' @rdname dictS3
#' @export
popitem.Dict <- function(x) x$popitem()

#' @rdname dictS3
#' @export
remove.Dict <- function(x, key, ...) x$remove(key)

#' @rdname dictS3
#' @export
setval.Dict <- function(x, key, value, add=FALSE, ...) x$set(key, value, add)

#' @rdname dictS3
#' @export
sortkey.Dict <- function(x, decr=FALSE, ...) x$sort(decr)


#' Binary `Dict` operators
#'
#' @description Binary operators for [Dict()] objects.
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
    if (!is.character(key) || length(key) != 1) {
        stop("cannot select more than one element")
    }
    if (missing(default)) {
        x$get(key)
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
            x$get(k)
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
    if (!is.character(key) || length(key) != 1) {
        stop("cannot set more than one element")
    }
    x$set(key, value, add)
}


#' @rdname dictS3replace
#' @return For `[<-` replaces the values at the given `keys`.
#' @export
`[<-.Dict` <- function(x, key, add = FALSE, value)
{
    if (length(key) != length(value)) {
        stop("length of key and value must match")
    }

    set_value = function(key, value) x$set(key, value, add)
    mapply(key, value, FUN = set_value)
    invisible(x)
}

