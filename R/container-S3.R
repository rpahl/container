#' @export add clear discard empty has remove size type values
add <- function(x, ...) UseMethod("add")
clear <- function(x) UseMethod("clear")
clone <- function(x, ...) UseMethod("clone")
discard <- function(x, ...) UseMethod("discard")
empty <- function(x) UseMethod("empty")
has <- function(x, ...) UseMethod("has")
remove <- function(x, ...) UseMethod("remove")
size <- function(x) UseMethod("size")
type <- function(x) UseMethod("type")
values <- function(x) UseMethod("values")

#' @title Container constructors 
#' @name ContainerS3
#' @param x (vector or list) initial elements of the container
NULL

#' @rdname ContainerS3 
#' @details \code{container(x)}: initialize \code{\link[container]{Container}}
#'  object. The type of \code{x} determines the internal storage mode.
#' @export
container <- function(x=list()) Container$new(x)

#' @rdname ContainerS3 
#' @details \code{as.container(x)}: convert named list to container
#' @export
as.container <- function(x) container(x)

#' @rdname ContainerS3 
#' @details \code{is.container(x)}: check for container type
#' @export
is.container <- function(x) inherits(x, "Container")


#' @title Container S3 member functions
#' @name ContainerS3funcs
#' @param cont the container object
#' @param elem a container element
NULL

#' @rdname ContainerS3funcs
#' @details add(cont, elem): Add \code{elem} to \code{cont}.
#' @export
add.Container <- function(cont, elem) cont$add(elem)

#' @rdname ContainerS3funcs
#' @details clear(cont): Remove all elements from \code{cont}.
#' @export
clear.Container <- function(cont, clear) cont$clear()

#' @rdname ContainerS3funcs
#' @details clone(cont, deep=FALSE): Create a copy of \code{cont} object. For
#'  more details see documentation of \code{\link[R6]{R6Class}}. 
#' @export
clone.Container <- function(cont, deep=FALSE) cont$clone(deep)

#' @rdname ContainerS3funcs
#' @details discard(cont, elem, right=FALSE): Search for first \code{elem} in
#'  \code{cont} and, if found, remove it. If \code{right} is \code{TRUE},
#'  search from right to left.
#' @export
discard.Container <- function(cont, elem, right=FALSE) cont$discard(elem, right)

#' @rdname ContainerS3funcs
#' @details empty(cont): Return \code{TRUE} if \code{container} is empty, else
#'  \code{FALSE}.
#' @export
empty.Container <- function(cont, empty) cont$empty()

#' @rdname ContainerS3funcs
#' @details has(cont, elem): Return \code{TRUE} if \code{cont} contains
#'  \code{elem} else \code{FALSE}.
#' @export
has.Container <- function(cont, has, elem) cont$has(elem)

#' @export
size.Container <- function(cont) cont$size()


#' @rdname ContainerS3funcs
#' @details discard(cont, elem, right): Search for first \code{elem} in
#'  \code{cont} and, if found, remove it. If \code{right} is
#'  \code{TRUE}, search from right to left.
#' @param right (logical) if TRUE, search from right to left
#' @export
discard.Container <- function(cont, elem, right=FALSE) cont$discard(elem, right)


#' @export
has <- function(x, ...) UseMethod("has")

#' @rdname ContainerS3funcs
#' @details has(cont, elem): Return \code{TRUE} if \code{Container} contains
#'  \code{elem} else \code{FALSE}.
#' @export
has.Container <- function(cont, elem) cont$has(elem)

#' @rdname ContainerS3funcs
#' @param list.len (integer) maximum number of list elements to display within
#'  a level.
#' @param ... (list) further arguments passed to \code{\link[utils]{str}}
#' @details print(cont, list.len=10L, ...): 
#' @export
print.Container <- function(cont, list.len=10, ...) cont$print(list.len, ...)

#' @rdname ContainerS3funcs
#' @details remove(cont, elem, right=FALSE): Same as \code{discard}, but throw
#'  an error if \code{elem} is not found.
#' @export
remove.Container <- function(cont, elem, right=FALSE) cont$remove(elem, right)

#' @rdname ContainerS3funcs
#' @details size(cont): Return size of the \code{cont}.
#' @export
size.Container <- function(cont) cont$size()

#' @rdname ContainerS3funcs
#' @details type(cont): Return type (or mode) of internal vector containing
#'  the elements.
#' @export
type.Container <- function(cont) cont$type()

#' @rdname ContainerS3funcs
#' @details values(cont): Return a copy of all elements in the same format as
#'  they are stored in the object.
#' @export
values.Container <- function(cont) cont$values()


#' @title Container operators
#' @name ContainerS3op
#' @param c1 \code{\link[container]{Container}} object
#' @param c2 \code{\link[container]{Container}} object
#' @return \code{\link[container]{Container}} object
NULL

#' @rdname ContainerS3op
#' @export
#' @details \code{c1 + c2}: return \code{c1} and \code{c2} combined (as a copy)
`+.Container` <- function(c1, c2) c1$clone()$add(c2)


# Conversion

#' @export
`as.vector.Container` <- function(x, mode="any") as.vector(x$values(), mode=mode)

#' @export
`as.list.Container` <- function(x) as.list(x$values())


