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
dict <- function(x=list()) 
{
    Dict$new(as.list(x))
}

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
#' @return element found at key, or default if not found.
#' @export
`[[.Dict` <- function(d, key, default=NULL)
{
    d$peek(key, default)
}

