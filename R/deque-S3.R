#' Deque (double-ended queue) S3 interface
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the [deque()] can also be used to mimic
#' both stacks and queues.
#' @details For a detailed documentation of all methods see [Deque()].
#' @name dequeS3
#' @seealso [Container()], [`+.Deque()`]
NULL

# S3 generic methods not derived from container

#' @rdname dequeS3
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname dequeS3
#' @export
count <- function(x, ...) UseMethod("count")

#' @rdname dequeS3
#' @export
peek <- function(x, ...) UseMethod("peek")

#' @rdname dequeS3
#' @export
peekleft <- function(x) UseMethod("peekleft")

#' @rdname dequeS3
#' @export
pop <- function(x, ...) UseMethod("pop")

#' @rdname dequeS3
#' @export
popleft <- function(x) UseMethod("popleft")

#' @rdname dequeS3
#' @export
reverse <- function(x) UseMethod("reverse")

#' @rdname dequeS3
#' @export
rotate <- function(x, ...) UseMethod("rotate")


#' @rdname dequeS3
#' @export
deque <- function(x=list()) Deque$new(x)

#' @rdname dequeS3
#' @export
as.deque <- function(x) Deque$new(x)

#' @rdname dequeS3
#' @export
is.deque <- function(x) inherits(x, "Deque")

#' @rdname dequeS3
#' @export
addleft.Deque <- function(x, elem, ...) x$addleft(elem)

#' @rdname dequeS3
#' @export
count.Deque <- function(x, elem, ...) x$count(elem)

#' @rdname dequeS3
#' @export
peek.Deque <- function(x, ...) x$peek()

#' @rdname dequeS3
#' @export
peekleft.Deque <- function(x) x$peekleft()

#' @rdname dequeS3
#' @export
pop.Deque <- function(x, ...) x$pop()

#' @rdname dequeS3
#' @export
popleft.Deque <- function(x) x$popleft()

#' @rdname dequeS3
#' @export
reverse.Deque <- function(x) x$reverse()

#' @rdname dequeS3
#' @export
rotate.Deque <- function(x, n = 1L, ...) x$rotate(n)


#' Binary `Deque` operators
#'
#' @description Binary operators for [Deque()] objects.
#' @name dequeS3binOp
#'
#' @param x1 primitive or [Deque()] object
#' @param x2 primitive or [Deque()] object
NULL

#' @rdname dequeS3binOp
#' @return For `+` if left side is a [Deque()], adds element to right side of
#' the [Deque()]. If right side is a [Deque()], adds element to left side of
#' the [Deque()].
#' @export
`+.Deque` <- function(x1, x2)
{
    if (is.deque(x1)) {
        x1$clone()$add(x2)
    } else {
        x2$clone()$addleft(x1)
    }
}

