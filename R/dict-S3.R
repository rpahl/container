#' @title Dictionary constructors 
#' @name dictS3
#' @param x a named list (or an object convertible to such, e.g.
#' \code{\link[base]{data.frame}}) with unique names.
#' @return \code{\link[container]{Dict}} object
NULL


#' @rdname dictS3 
#' @details \code{dict(x)}: initializes a \code{\link[container]{Dict}} object with
#'  all list elements inserted in the dict.
#' @export
dict <- function(x=list()) Dict$new(as.list(x))

#' @rdname dictS3 
#' @details \code{as.dict(x)}: convert named list to dictionary
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


#' @title Dictionary operators
#' @name dictS3op
#' @param d1 \code{\link[container]{Dict}} object
#' @param d2 \code{\link[container]{Dict}} object
#' @return \code{\link[container]{Dict}} object
NULL

#' @rdname dictS3op 
#' @details \code{d1 + d2}: returns a copy of combining \code{d1} and \code{d2}
#' @export
`+.Dict` <- function(d1, d2)
{
    d1$clone()$update(d2)
}

#' @rdname dictS3op 
#' @details \code{d1 - d2}: creates a copy of \code{d1}, removes any keys
#'  occuring in \code{d2} and returns the copy.
#' @export
`-.Dict` <- function(d1, d2)
{
    d1.clone <- d1$clone()
    lapply(d2$keys(), FUN = function(k) d1.clone$discard(k))
    d1.clone
}

#' @title Set value at key
#' @param d \code{\link[container]{Dict}} object
#' @param key (character) the key
#' @param add (logical) if TRUE, value is added if not yet in dict. If FALSE
#'  and value not yet in dict, an error is signaled.
#' @return updated \code{\link[container]{Dict}} object
#' @export
`[<-.Dict` <- function(d, key, add=FALSE, value)
{
    d$set(key, value, add)
}

#' @title Get value at key
#' @details \code{d[key]} retrieve value at key.
#' @return element found at key
#' @export
`[.Dict` <- function(d, key)
{
    d$get(key)
}

#' @title Peek value at key
#' @details \code{d[key, default=NULL]} retrieve value at key. If key not
#' found, return default.
#' @return element found at key, or \code{default} if not found.
#' @export
`[[.Dict` <- function(d, key, default=NULL)
{
    d$peek(key, default)
}


#' @title Dictionary S3 member functions
#' @name DictS3funcs
#' @param dict the dict object
#' @param key (character) the key in the dictionary
#' @param value the value associated with the key


#' @rdname DictS3funcs
#' @details add(dict, key, value): If \code{key} not yet in \code{dict}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
#' @export
add.Dict <- function(dict, key, value) dict$add(key, value)


#' @rdname DictS3funcs
#' @details discard(dict, key): If \code{key} in \code{dict}, return a copy of
#'  its value and discard it afterwards.
#' @export
discard.Dict <- function(dict, key) dict$discard(key)

#' @rdname DictS3funcs
#' @details has(dict, key): TRUE if \code{key} in \code{dict} else FALSE.
#' @export
has.Dict <- function(dict, key) dict$has(key)

#' @rdname DictS3funcs
#' @details remove(dict, key): Same as \code{discard}, except that an error is
#'  thrown if \code{key} is not found in \code{dict}.
#' @export
remove.Dict <- function(dict, key) dict$remove(key)

#' @export
set <- function(x, ...) UseMethod("set")

#' @rdname DictS3funcs
#' @details set(dict, key, value): Like \code{add} but overwrites value
#' (instead of error) if \code{key} is already in \code{dict}. If \code{key}
#'  not in \code{dict}, an error is thrown unless \code{add == TRUE}.
#' @param add (logical) if TRUE, set value even if key does not exist
#' @export
set.Dict <- function(dict, key, value, add=FALSE) dict$set(key, value, add)

