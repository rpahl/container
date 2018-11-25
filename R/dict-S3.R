#' @export keys peek pop popitem
keys <- function(x) UseMethod("keys")
peek <- function(x, ...) UseMethod("peek")
pop <- function(x, ...) UseMethod("pop")
popitem <- function(x) UseMethod("popitem")

#' @title Dict constructors 
#' @name dictS3
#' @param x (named list or data.frame) initial elements of the \code{dict}
#' @return \code{\link[container]{Dict}} object
NULL

#' @rdname dictS3 
#' @details \code{dict(x=list())}: create a \code{\link[container]{Dict}} object
#' @export
dict <- function(x=list()) Dict$new(x)

#' @rdname dictS3 
#' @details \code{as.dict(x)}: convert x to dictionary
#' @export
as.dict <- function(x) dict(x)

#' @rdname dictS3 
#' @details \code{is.dict(x)}: check for dictionary type
#' @export
is.dict <- function(x) inherits(x, "Dict")


#' @title Convert \code{\link[container]{Dict}} to \code{\link[base]{data.frame}}
#' @export
`as.data.frame.Dict` <- function(d1) 
{
    as.data.frame(as.list(d1))
}

#' @title Container S3 member functions overwritten by Dict
#' @name ContainerS3byDict
#' @param dict The dict object.
#' @param key (character) The key in the dictionary.
#' @param value The value associated with the key.
NULL

#' @rdname ContainerS3byDict
#' @details add(dict, key, value): If \code{key} not yet in \code{dict}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
#' @export
add.Dict <- function(dict, key, value) dict$add(key, value)

#' @rdname ContainerS3byDict
#' @details discard(dict, key): If \code{key} in \code{dict}, return a copy of
#'  its value and discard it afterwards.
#' @export
discard.Dict <- function(dict, key) dict$discard(key)

#' @rdname ContainerS3byDict
#' @details has(dict, key): TRUE if \code{key} in \code{dict} else FALSE.
#' @export
has.Dict <- function(dict, key) dict$has(key)

#' @rdname ContainerS3byDict
#' @details remove(dict, key): Same as \code{discard}, except that an error is
#'  thrown if \code{key} is not found in \code{dict}.
#' @export
remove.Dict <- function(dict, key) dict$remove(key)


#' @title Dict S3 member functions
#' @name DictS3funcs
#' @param dict The dict object.
#' @param key (character) The key in the dictionary.
#' @param value The value associated with the key.
NULL

#' @rdname DictS3funcs
#' @details keys(dict): Return all keys.
#' @export
keys.Dict <- function(dict) dict$keys()

#' @rdname DictS3funcs
#' @details peek(dict, key, default=NULL): Return the value for \code{key} if
#'  \code{key} is in \code{dict}, else \code{default}.
#' @export
peek.Dict <- function(dict, key, default=NULL) dict$peek(key, default)

#' @rdname DictS3funcs
#' @details pop(dict, key): If \code{key} in \code{dict}, return a copy of its
#'  associated value and remove it from the \code{dict}.
#' @export
pop.Dict <- function(dict, key) dict$pop(key)

#' @rdname DictS3funcs
#' @details popitem(dict): Remove and return an arbitrary (key, value) pair
#'  from the dictionary. \code{popitem()} is useful to destructively iterate
#'  over a \code{dict}, as often used in set algorithms.
#' @export
popitem.Dict <- function(dict) dict$popitem()


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
#' @param dict \code{\link[container]{Dict}} object
#' @param key (character) the key
#' @param add (logical) if TRUE, value is added if not yet in dict. If FALSE
#'  and value not yet in dict, an error is signaled.
#' @return updated \code{\link[container]{Dict}} object
#' @export
`[[<-.Dict` <- function(dict, key, add=FALSE, value) dict$set(key, value, add)

#' @title Get value at key
#' @details \code{dict[key]}: If \code{key} in \code{dict}, return value, else
#'  throw key-error.
#' @return value at key
#' @export
`[[.Dict` <- function(dict, key) dict$get(key)

#' @title Add value at key
#' @details \code{dict[key] <- value}: If \code{key} not yet in \code{dict}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
#' @export
`[<-.Dict` <- function(dict, key, value) dict$add(key, value)

#' @title Peek value at key
#' @details \code{dict[key, default=NULL]}: Return the value for \code{key} if
#'  \code{key} is in \code{dict}, else \code{default}.
#' @return element found at key, or \code{default} if not found.
#' @export
`[.Dict` <- function(dict, key, default=NULL) dict$peek(key, default)


