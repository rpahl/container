#' @title Container constructors 
#' @name ContainerS3
#' @param x a named list (or an object convertible to such, e.g.
#' \code{\link[base]{data.frame}}) with unique names.
#' @return \code{\link[container]{Container}} object
NULL


#' @rdname ContainerS3 
#' @details \code{dict(x)}: initializes a \code{\link[container]{Container}}
#'  object with all list elements inserted in the container.
#' @export
container <- function(x=list()) Container$new(as.list(x))

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

#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname ContainerS3funcs
#' @details add(cont, elem): Add \code{elem} to \code{cont}.
#' @export
add.Container <- function(cont, elem) cont$add(elem)


#' @export
discard <- function(x, ...) UseMethod("discard")


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


#' @export
remove <- function(x, ...) UseMethod("remove")

#' @rdname ContainerS3funcs
#' @details remove(cont, elem): Same as \code{discard}, but throw an
#'  error if \code{elem} is not found.
#' @export
remove.Container <- function(cont, elem, right=FALSE) cont$remove(elem, right)
