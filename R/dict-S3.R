#' @title Dict constructors 
#' @description The \code{dict} resembles Python's dict type, and is implemented
#' as a specialized associative (or mapping) \code{\link[container]{container}} thus
#' sharing all \code{\link[container]{container}} methods with some of them being
#' overriden to account for the associative key-value pair semantic.
#' @name dictS3
#' @param x (named list or data.frame) initial elements of the \code{dict}
#' @return \code{\link[container]{Dict}} object
#' @seealso \code{\link[container]{container}}, \code{\link[container]{Dict}},
#'  \code{\link[container]{+.Dict}}, 
#'  \code{\link[container]{[<-.Dict}},
#'  \code{\link[container]{[[<-.Dict}},
#'  \code{\link[container]{[[.Dict}},
#'  \code{\link[container]{[.Dict}}
#' @export dict as.dict is.dict getval keys popitem setval sortkey
#' 
#' @section S3 methods for Dict objects:
#' \describe{
#'  \item{\code{add(dic, key, value)}}{If \code{key} not yet in \code{dic},
#'      insert \code{value} at \code{key}, otherwise signal an error.}
#'  \item{\code{discard(dic, key)}}{If \code{key} in \code{dic}, remove it.}
#'  \item{\code{has(dic, key)}}{TRUE if \code{key} in \code{dic} else FALSE.}
#'  \item{\code{remove(dic, key)}}{If \code{key} in \code{dic}, remove it,
#'      otherwise raise an error.}
#'  \item{\code{getval(dic)}}{If \code{key} in \code{dic}, return value, else
#'      throw key-error.}
#'  \item{\code{keys(dic)}}{Return a character vector of all keys.}
#'  \item{\code{peek(dic, key, default=NULL)}}{Return the value for \code{key} if
#'      \code{key} is in the \code{dic}, else \code{default}.}
#'  \item{\code{pop(dic, key)}}{If \code{key} in \code{dic}, return a copy of its
#'      value and discard it afterwards.}
#'  \item{\code{popitem(dic)}}{Remove and return an arbitrary (key, value) pair
#'  from the dictionary. \code{popitem()} is useful to destructively iterate
#'  over a \code{dic}, as often used in set algorithms.}
#'  \item{\code{setval(dic, key, value, add=FALSE)}}{Like \code{add} but overwrites
#'      value if \code{key} is already in the \code{dic}. If \code{key} not in
#'      \code{dic}, an error is thrown unless \code{add} was set to
#'      \code{TRUE}.}
#'  \item{\code{sortkey(decr=FALSE)}}{Sort values in dictionary according to keys.}
#'  \item{\code{update(other=dict())}}{Adds element(s) of other to the 
#'      dictionary if the key(s) are not in the dictionary and updates all keys with
#'      the new value(s) otherwise.}
#' }
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

#' @rdname dictS3 
getval <- function(x, ...) UseMethod("getval")

#' @rdname dictS3 
keys <- function(x) UseMethod("keys")

#' @rdname dictS3 
popitem <- function(x) UseMethod("popitem")

#' @rdname dictS3 
setval <- function(x, ...) UseMethod("setval")

#' @rdname dictS3 
sortkey <- function(x, ...) UseMethod("sortkey")

#' @rdname dictS3 
#' @param dict The dict object.
#' @param key (character) The key in the dictionary.
#' @param value The value associated with the key.
#' @export
add.Dict <- function(dic, key, value) dic$add(key, value)

#' @rdname dictS3 
#' @export
discard.Dict <- function(dic, key) dic$discard(key)

#' @rdname dictS3 
#' @export
has.Dict <- function(dic, key) dic$has(key)

#' @rdname dictS3 
#' @export
remove.Dict <- function(dic, key) dic$remove(key)

#' @rdname dictS3 
#' @export
getval.Dict <- function(dic, key) dic$get(key)

#' @rdname dictS3 
#' @export
keys.Dict <- function(dic) dic$keys()

#' @rdname dictS3 
#' @export
peek.Dict <- function(dic, key, default=NULL) dic$peek(key, default)

#' @rdname dictS3 
#' @export
pop.Dict <- function(dic, key) dic$pop(key)

#' @rdname dictS3 
#' @export
popitem.Dict <- function(dic) dic$popitem()

#' @rdname dictS3 
#' @export
setval.Dict <- function(dic, key, value, add=FALSE) dic$set(key, value, add)

#' @rdname dictS3 
#' @export
sortkey.Dict <- function(dic, decr=FALSE) dic$sort(decr)

#' @rdname dictS3 
#' @export
update.Dict <- function(dic, other=dict()) dic$update(other)


#' @title Binary dict operators
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
#' @name dictS3replace
#' @param dic \code{\link[container]{Dict}} object
#' @param key (character) the key
#' @param add (logical) if TRUE, value is added if not yet in dict. If FALSE
#'  and value not yet in dict, an error is signaled.
#' @return updated \code{\link[container]{Dict}} object
#' @export
`[[<-.Dict` <- function(dic, key, add=FALSE, value) dic$set(key, value, add)

#' @rdname dictS3replace
#' @details \code{dic[key] <- value}: If \code{key} not yet in \code{dic}, insert
#'  \code{value} at \code{key}, otherwise raise an error.
#' @export
`[<-.Dict` <- function(dic, key, value) dic$add(key, value)

#' @rdname dictS3replace
#' @details \code{dic[key]}: If \code{key} in \code{dic}, return value, else
#'  throw key-error.
#' @return value at key
#' @export
`[[.Dict` <- function(dic, key) dic$get(key)

#' @rdname dictS3replace
#' @details \code{dic[key, default=NULL]}: Return the value for \code{key} if
#'  \code{key} is in \code{dic}, else \code{default}.
#' @return element found at key, or \code{default} if not found.
#' @export
`[.Dict` <- function(dic, key, default=NULL) dic$peek(key, default)


