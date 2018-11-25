#' @title Container S3 interface 
#' @description This function creates a container data structure with typical
#' member functions to insert, delete and access objects from the container. It
#' also serves as the base class for objects created with 
#'  \code{\link[container]{deque}}, \code{\link[container]{set}}, and 
#'  \code{\link[container]{dict}}.
#' @details
#' The underlying data structure is based on R vectors (or lists), with the mode
#' being set to the mode (or type) of the value passed to the initialize
#' function, which by default is an empty list, in which case the
#' \code{Container} object can store objects of mixed and arbitrary types.
#' If the container will only contain objects of one particular type, for
#' example, double values, it will be both more efficient and type safe to
#' initialize the container using this particular type (see Examples section).
#' @name ContainerS3
#' @param x (vector or list) initial elements of the container
#' @param cont the container object
#' @param elem a container element
#' @return \code{\link[container]{Container}} object
#' @seealso \code{\link[container]{Container}}, \code{\link[container]{+.Container}}, 
#' @export container as.container is.container
#' @export add clear discard empty has remove size type values
#'
#' @section S3 methods for Container objects:
#' \describe{
#'  \item{\code{add(cont, elem)}}{Add \code{elem} to \code{cont}.}
#'  \item{\code{clear(cont)}}{Remove all elements from the \code{cont}.}
#'  \item{\code{clone(cont)}}{Create a copy of \code{cont} object. For
#'  more details see documentation of \code{\link[R6]{R6Class}}.}
#'  \item{\code{discard(cont, elem, right=FALSE)}}{Search for first \code{elem} in
#'      \code{cont} and, if found, remove it. If \code{right} is
#'      \code{TRUE}, search from right to left.}
#'  \item{\code{empty(cont)}}{Return \code{TRUE} if the \code{cont} is empty,
#'      else \code{FALSE}.}
#'  \item{\code{has(cont, elem)}}{Return \code{TRUE} if \code{cont} contains
#'      \code{elem} else \code{FALSE}.}
#'  \item{\code{print(cont, list.len, ...)}}{Print container object representation
#'      similar to \code{\link[utils]{str}}}
#'  \item{\code{remove(cont, elem, right=FALSE)}}{Same as \code{discard}, but throw an
#'      error if \code{elem} does not exist.}
#'  \item{\code{size(cont)}}{Return size of the \code{cont}.}
#'  \item{\code{type(cont)}}{Return type (or mode) of internal vector containing
#'  the elements of the container.}
#'  \item{\code{values(cont)}}{Return a copy of all elements in the same format
#'  as they are stored in the object.}
#' }
#' @examples
#' c0 <- container(list(2, "A"))
#' size(c0)                         # 2
#' add(c0, 1)
#' c0$has(2)                        # TRUE
#' discard(c0, 2)
#' has(c0, 2)                       # FALSE
#' 
#' \dontrun{
#' c0$remove(2)                     # Error : 2 not in Container
#' }
#' discard(c0, 2)                   # ok (no effect)
#' 
# Container types
#' type(container(list("A", 1)))    # "list"
#' type(container(numeric(0)))      # "double"
#' type(container(0+0i))            # "complex"
#' type(container(letters[1:3]))    # "character"
#' values(container(letters[1:3]))  # "a" "b" "c"
#' type(container(1L))              # "integer"
#' values(add(container(1L), 2.3))  # since integer type, equals c(1, 2)
NULL

#' @rdname ContainerS3 
#' @details \code{container(x)}: initialize \code{\link[container]{Container}}
#'  object. The type of \code{x} determines the internal storage mode.
container <- function(x=list()) Container$new(x)

#' @rdname ContainerS3 
#' @details \code{as.container(x)}: convert x to \code{Container} object
as.container <- function(x) container(x)

#' @rdname ContainerS3 
#' @details \code{is.container(x)}: check for \code{Container} class
is.container <- function(x) inherits(x, "Container")

#' @rdname ContainerS3 
add <- function(x, ...) UseMethod("add")   

#' @rdname ContainerS3 
clear <- function(x) UseMethod("clear")

#' @rdname ContainerS3 
clone <- function(x, ...) UseMethod("clone")

#' @rdname ContainerS3 
discard <- function(x, ...) UseMethod("discard")

#' @rdname ContainerS3 
empty <- function(x) UseMethod("empty")

#' @rdname ContainerS3 
has <- function(x, ...) UseMethod("has")

#' @rdname ContainerS3 
remove <- function(x, ...) UseMethod("remove")

#' @rdname ContainerS3 
size <- function(x) UseMethod("size")

#' @rdname ContainerS3 
type <- function(x) UseMethod("type")

#' @rdname ContainerS3 
values <- function(x) UseMethod("values")

#' @rdname ContainerS3
add.Container <- function(cont, elem) cont$add(elem)

#' @rdname ContainerS3
clear.Container <- function(cont) cont$clear()

#' @rdname ContainerS3
clone.Container <- function(cont, deep=FALSE) cont$clone(deep)

#' @rdname ContainerS3
discard.Container <- function(cont, elem, right=FALSE) cont$discard(elem, right)

#' @rdname ContainerS3
empty.Container <- function(cont) cont$empty()

#' @rdname ContainerS3
has.Container <- function(cont, elem) cont$has(elem)

#' @rdname ContainerS3
#' @param list.len (integer) maximum number of list elements to display within
#'  a level.
#' @param ... (list) further arguments passed to \code{\link[utils]{str}}
print.Container <- function(cont, list.len=10, ...) cont$print(list.len, ...)

#' @rdname ContainerS3
remove.Container <- function(cont, elem, right=FALSE) cont$remove(elem, right)

#' @rdname ContainerS3
size.Container <- function(cont) cont$size()

#' @rdname ContainerS3
type.Container <- function(cont) cont$type()

#' @rdname ContainerS3
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

