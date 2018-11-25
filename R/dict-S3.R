#' @title Dict constructors 
#' @description The \code{dict} resembles Python's dict type, and is implemented
#' as a specialized associative (or mapping) \code{\link[container]{container}} thus
#' sharing all \code{\link[container]{container}} methods with some of them being
#' overriden to account for the associative key-value pair semantic.
#' @name dictS3
#' @param x (named list or data.frame) initial elements of the \code{dict}
#' @return \code{\link[container]{Dict}} object
#' @seealso \code{\link[container]{container}}, \code{\link[container]{Dict}}
#' @export dict as.dict is.dict
#' @examples
#' ages <- dict(c(Peter=24, Lisa=23, Bob=32))
#' has(ages, "Peter")   # TRUE
#' ages["Lisa"]   # 23
#' ages["Mike"]   # NULL
#' ages["Mike"] <- 18
#' ages["Mike"]   # 18
#' keys(ages)
#' print(ages)
#' 
#' \dontrun{
#' ages["Peter"] <- 24 + 1     # key 'Peter' already in Dict
#' dict(c(Peter=24, Peter=20)) # Error: duplicated keys
#' }
NULL

#' @rdname dictS3 
#' @details \code{dict(x=list())}: create a \code{\link[container]{Dict}} object
dict <- function(x=list()) Dict$new(x)

#' @rdname dictS3 
#' @details \code{as.dict(x)}: convert x to \code{Dict} object
as.dict <- function(x) dict(x)

#' @rdname dictS3 
#' @details \code{is.dict(x)}: check for \code{Dict} class
is.dict <- function(x) inherits(x, "Dict")


#' @export
`as.data.frame.Dict` <- function(d1) as.data.frame(as.list(d1))


#' @title Dict S3 member functions
#' @name DictS3funcs
#' @param dict The dict object.
#' @param key (character) The key in the dictionary.
#' @param value The value associated with the key.
#' @export keys popitem 
NULL

#' @rdname DictS3funcs
keys <- function(x) UseMethod("keys")
#' @rdname DictS3funcs
popitem <- function(x) UseMethod("popitem")

#' @rdname DictS3funcs
#' @details add(dic, key, value): If \code{key} not yet in \code{dic}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
add.Dict <- function(dic, key, value) dic$add(key, value)

#' @rdname DictS3funcs
#' @details discard(dic, key): If \code{key} in \code{dic}, return a copy of
#'  its value and discard it afterwards.
discard.Dict <- function(dic, key) dic$discard(key)

#' @rdname DictS3funcs
#' @details has(dic, key): TRUE if \code{key} in \code{dic} else FALSE.
has.Dict <- function(dic, key) dic$has(key)

#' @rdname DictS3funcs
#' @details remove(dic, key): Same as \code{discard}, except that an error is
#'  thrown if \code{key} is not found in \code{dic}.
remove.Dict <- function(dic, key) dic$remove(key)

#' @rdname DictS3funcs
#' @details keys(dic): Return all keys.
keys.Dict <- function(dic) dic$keys()

#' @rdname DictS3funcs
#' @details peek(dic, key, default=NULL): Return the value for \code{key} if
#'  \code{key} is in \code{dic}, else \code{default}.
peek.Dict <- function(dic, key, default=NULL) dic$peek(key, default)

#' @rdname DictS3funcs
#' @details pop(dic, key): If \code{key} in \code{dic}, return a copy of its
#'  associated value and remove it from the \code{dic}.
pop.Dict <- function(dic, key) dic$pop(key)

#' @rdname DictS3funcs
#' @details popitem(dic): Remove and return an arbitrary (key, value) pair
#'  from the dictionary. \code{popitem()} is useful to destructively iterate
#'  over a \code{dic}, as often used in set algorithms.
popitem.Dict <- function(dic) dic$popitem()


#' @title Binary dict operators
#' @name dictS3binOp
#' @param d1 \code{\link[container]{Dict}} object
#' @param d2 \code{\link[container]{Dict}} object
#' @return \code{\link[container]{Dict}} object
NULL

#' @rdname dictS3binOp 
#' @details \code{d1 + d2}: return \code{d1} and \code{d2} combined (as a copy)
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

#' @title Set value at key
#' @param dic \code{\link[container]{Dict}} object
#' @param key (character) the key
#' @param add (logical) if TRUE, value is added if not yet in dict. If FALSE
#'  and value not yet in dict, an error is signaled.
#' @return updated \code{\link[container]{Dict}} object
#' @export
`[[<-.Dict` <- function(dic, key, add=FALSE, value) dic$set(key, value, add)

#' @title Get value at key
#' @details \code{dic[key]}: If \code{key} in \code{dic}, return value, else
#'  throw key-error.
#' @return value at key
#' @export
`[[.Dict` <- function(dic, key) dic$get(key)

#' @title Add value at key
#' @details \code{dic[key] <- value}: If \code{key} not yet in \code{dic}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
#' @export
`[<-.Dict` <- function(dic, key, value) dic$add(key, value)

#' @title Peek value at key
#' @details \code{dic[key, default=NULL]}: Return the value for \code{key} if
#'  \code{key} is in \code{dic}, else \code{default}.
#' @return element found at key, or \code{default} if not found.
#' @export
`[.Dict` <- function(dic, key, default=NULL) dic$peek(key, default)


