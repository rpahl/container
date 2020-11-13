#' @title Container S3 interface
#''
#' @description This function creates a [Container()] object, which is a data
#' structure with typical member functions to insert, delete and access objects
#' from the container. Since the [Container()] class mainly serves as the
#' base class for [Deque()], [Set()], and [Dict()] objects, users are more
#' likley to use the corresponding [deque()], [set()], and [dict()] methods to
#' create objects of the respective derived classes.
#' @details For a detailed documentation of all methods see [Container()]
#' @param x a vector of 'any' type
#' @param ... further arguments depending on the method.
#' @name ContainerS3
#' @seealso [Container()], [`+.Container()`], [deque()], [set()], [dict()]
NULL

# S3 generic methods

#' @rdname ContainerS3
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname ContainerS3
#' @export
clear <- function(x) UseMethod("clear")

#' @rdname ContainerS3
#' @export
clone <- function(x, ...) UseMethod("clone")

#' @rdname ContainerS3
#' @export
discard <- function(x, ...) UseMethod("discard")

#' @rdname ContainerS3
#' @export
empty <- function(x) UseMethod("empty")

#' @rdname ContainerS3
#' @export
has <- function(x, ...) UseMethod("has")

#' @rdname ContainerS3
#' @export
remove <- function(x, ...) UseMethod("remove")

#' @rdname ContainerS3
#' @export
size <- function(x) UseMethod("size")

#' @rdname ContainerS3
#' @export
type <- function(x) UseMethod("type")

#' @rdname ContainerS3
#' @export
values <- function(x) UseMethod("values")

#' @rdname ContainerS3
#' @export
container <- function(x = list()) Container$new(x)

#' @rdname ContainerS3
#' @export
as.container <- function(x) container(x)

#' @rdname ContainerS3
#' @export
is.container <- function(x) inherits(x, "Container")

#' @rdname ContainerS3
#' @export
iter.Container <- function(x) x$iter()

#' @rdname ContainerS3
#' @param elem some element of the container
#' @export
add.Container <- function(x, elem, ...) x$add(elem)

#' @rdname ContainerS3
#' @export
clear.Container <- function(x) x$clear()

#' @rdname ContainerS3
#' @param deep `logical` if `TRUE` a `deep` copy is done otherwise by default a
#' shallow copy is performed.
#' @export
clone.Container <- function(x, deep=FALSE, ...) x$clone(deep)

#' @rdname ContainerS3
#' @param right `logical` if `TRUE` search from right to left
#' @export
discard.Container <- function(x, elem, right=FALSE, ...) x$discard(elem, right)

#' @rdname ContainerS3
#' @export
empty.Container <- function(x) x$empty()

#' @rdname ContainerS3
#' @export
has.Container <- function(x, elem, ...) x$has(elem)

#' @rdname ContainerS3
#' @param list.len `integer` number of elements to print
#' @export
print.Container <- function(x, list.len=10, ...) x$print(list.len, ...)

#' @export
remove.Container <- function(x, elem, right=FALSE, ...) x$remove(elem, right)

#' @rdname ContainerS3
#' @export
size.Container <- function(x) x$size()

#' @rdname ContainerS3
#' @export
type.Container <- function(x) x$type()

#' @rdname ContainerS3
#' @export
values.Container <- function(x) x$values()


#' @title Binary `Container` operators
#' @description Binary operators for [Container()] objects.
#' @name ContainerS3op
NULL

#' @rdname ContainerS3op
#' @param c1 [Container()] object
#' @param c2 [Container()] object
#' @return For `+` returns `c1` and `c2` combined (as a copy)
#' @export
`+.Container` <- function(c1, c2) c1$clone()$add(c2)



# Conversion to standard R objects

#' @export
`as.vector.Container` <- function(x, mode = "any")
{
    as.vector(x$values(), mode = mode)
}

#' @export
`as.list.Container` <- function(x, ...) as.list(x$values())

