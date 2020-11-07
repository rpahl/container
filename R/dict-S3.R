#' Dict S3 methods
#'
#' @description The `Dict` resembles Python's dict type, and is implemented
#' as a specialized associative `Container` thus sharing all `Container` methods
#' with some of them being overriden to account for the associative key-value
#' pair semantic.
#' @name dictS3
#' @param x initial elements passed to constructor or object of class `Dict`
#' passed to member methods.
#' @param ... further arguments
#'
#' @section S3 methods for class `Dict`:
#'  * `add(x, key, value)` if `key` not yet in `x`, insert `value` at `key`,
#'    otherwise signal an error.
#'  * `discard(x, key)` if `key` in `x`, remove it.
#'  * `getval(x)` if `key` in `x`, return `value`, else throw key-error.
#'  * `has(x, key)` `TRUE` if `key` in `x` else `FALSE`.
#'  * `keys(x)` return a character vector of all keys.
#'  * `names(x)` alias for `keys()`
#'  * `peek(x, key, default = NULL)` return the value for `key` if `key` is in
#'     the `x`, else `default`.
#'  * `pop(x, key)` if `key` in `x`, return a copy of its value and discard it
#'     afterwards.
#'  * `popitem(x)` remove and return an arbitrary (key, value) pair
#'    from the dictionary. This function can be useful to destructively iterate
#'    over the dictionary as often used in set algorithms.
#'  * `remove(x, key)` if `key` in `x`, remove it, otherwise raise an error.
#'  * `setval(x, key, value, add = FALSE)` Overrides `value` at `key` if `key`
#'     is already in the `Dict`. If `key` not in `Dict`, an error is thrown
#'     unless `add` was set to `TRUE`.
#'  *  `sortkey(x, decr = FALSE)` re-order elements according to key-order.
#'  *  `update(x, other = dict())` add elements of other dict to the `Dict`
#'     if the key is not in the `Dict` and update the key with the new value
#'     otherwise.
#'
#' @examples
#' ages <- dict(c(Peter=24, Lisa=23, Bob=32))
#' has(ages, "Peter")   # TRUE
#' ages["Lisa"]         # 23
#' ages["Mike"]         # NULL
#' ages["Mike"] <- 18
#' ages["Mike"]         # 18
#' keys(ages)
#' print(ages)
#'
#' \dontrun{
#' ages["Peter"] <- 24 + 1     # key 'Peter' already in Dict
#' dict(c(Peter=24, Peter=20)) # Error: duplicated keys
#' }
NULL

#' @rdname dictS3
#' @export
dict <- function(x = list())
{
    d <- Dict$new(x)
    if (is.data.frame(x)) {
        attr(d, "row.names") <- attr(x, "row.names")
    }
    d
}

#' @rdname dictS3
#' @export
as.dict <- function(x) dict(x)

#' @rdname dictS3
#' @export
is.dict <- function(x) inherits(x, "Dict")

#' @rdname dictS3
#' @export
getval <- function(x, ...) UseMethod("getval")

#' @rdname dictS3
#' @export
keys <- function(x) UseMethod("keys")

#' @rdname dictS3
#' @export
popitem <- function(x) UseMethod("popitem")

#' @rdname dictS3
#' @export
setval <- function(x, ...) UseMethod("setval")

#' @rdname dictS3
#' @export
sortkey <- function(x, ...) UseMethod("sortkey")


#' @export
add.Dict <- function(x, key, value, ...) x$add(key, value)

#' @export
discard.Dict <- function(x, key, ...) x$discard(key)

#' @export
getval.Dict <- function(x, key, ...) x$get(key)

#' @export
has.Dict <- function(x, key, ...) x$has(key)

#' @export
keys.Dict <- function(x) x$keys()

#' @export
names.Dict <- function(x) keys(x)

#' @export
peek.Dict <- function(x, key, default=NULL, ...) x$peek(key, default)

#' @export
pop.Dict <- function(x, key, ...) x$pop(key)

#' @export
popitem.Dict <- function(x) x$popitem()

#' @export
remove.Dict <- function(x, key, ...) x$remove(key)

#' @export
setval.Dict <- function(x, key, value, add=FALSE, ...) x$set(key, value, add)

#' @export
sortkey.Dict <- function(x, decr=FALSE, ...) x$sort(decr)

#' @export
update.Dict <- function(object, other=dict(), ...) object$update(other)


#' @title Binary dict operators
#' @description Binary operators for \code{Dict} objects.
#' @name dictS3binOp
#' @param d1 \code{\link[container]{Dict}} object
#' @param d2 \code{\link[container]{Dict}} object
#' @return \code{\link[container]{Dict}} object
NULL

#' @rdname dictS3binOp
#' @details \code{d1 + d2}: return a copy of \code{d1} updated by \code{d2}.
#' @export
`+.Dict` <- function(d1, d2) d1$clone()$update(d2)

#' @rdname dictS3binOp
#' @details \code{d1 - d2}: return a copy of \code{d1} with all keys being
#'  removed that occured in \code{d2}.
#' @export
`-.Dict` <- function(d1, d2)
{
    d1.clone <- d1$clone()
    lapply(d2$keys(), FUN = function(k) d1.clone$discard(k))
    d1.clone
}


#' @title Extract or replace \code{Dict} values
#' @description Access and assignment operators for \code{Dict} objects.
#' @name dictS3replace
#' @param x \code{\link[container]{Dict}} object
#' @param key (character) the key
#' @param add (logical) if TRUE, value is added if not yet in dict. If FALSE
#'  and value not yet in dict, an error is signaled.
#' @param value the value associated with the \code{key}
#' @return updated \code{\link[container]{Dict}} object
#' @export
`[[<-.Dict` <- function(x, key, add=FALSE, value) x$set(key, value, add)


#' @rdname dictS3replace
#' @param default the default value
#' @details `x[[key, default=NULL]]`: return the value for `key` if
#'  `key` is in `x`, else `default`.
#' @return element found at key, or `default` if not found.
#' @export
`[[.Dict` <- function(x, key, default=NULL) x$peek(key, default)


#' @rdname dictS3replace
#' @details `x[key] <- value`: if `key` not yet in `x`,
#'  insert `value` at `key`, otherwise raise an error.
#' @export
`[<-.Dict` <- function(x, key, value)
{
    x$add(key, value)
}


#' @rdname dictS3replace
#' @param i
#' @param j
#' @details `x[i, j]`:
#' @return
#' @export
`[.Dict` <- function(x, i, j)
{
    has.i <- !missing(i)
    has.j <- !missing(j)
    hasComma <- nargs() == 3
    hasRownames = !is.null(attr(x, "row.names"))

    if (has.i) {
        if (!hasComma) {
            # x[i]
            if (hasRownames && is.numeric(i)) {
                return(as.data.frame(x$values())[i])
            } else {
                return(x$get(i))
            }
        }
        # x[i, j]
        if (!hasRownames) {
            stop("Rows may only be selected if dict was initialized with a ",
                 "data.frame")
        }
        # Behave like a data.frame with selected rows (and optionally) columns
        as.data.frame(x$values())[i, j]
    } else {
        # x[, j]
        if (hasRownames && is.numeric(j)) {
            # Behave like data.frame with columns selected by index
            as.data.frame(x$values())[, j]
        } else {
            x$get(j)
        }
    }
}

