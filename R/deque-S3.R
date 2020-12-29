#' Deque (double-ended queue)
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the [deque()] can also be used to mimic
#' both stacks and queues.
#' @details For a full list of all deque methods see [Deque()].
#' @param ... initial elements put into the `Deque`.
#' @param keep_names `logical` if TRUE, keeps names of passed elements.
#' @param x any `R` object, or an object inheriting from class 'Deque' or
#' 'Container' for the corresponding S3 methods.
#' @return [deque()] returns a [Deque()] object.
#' @seealso [Deque()], [container()]
#' @name dequeS3
#' @export
deque <- function(..., keep_names = FALSE) {
    if (missing(keep_names)) {
        Deque$new(...)
    } else {
        Deque$new(..., keep_names = keep_names)
    }
}


#' @rdname dequeS3
#' @return [as.deque()] coerces to a deque.
#' @export
as.deque <- function(x, ...)
{
    if (is.null(x)) return(deque())
    UseMethod("as.deque")
}

#' @rdname dequeS3
#' @export
as.deque.Container <- function(x, ...)
{
    deque(values(x), ...)
}

#' @export
as.deque.default <- function(x, ...)
{
    if (is.deque(x)) return(x)
    deque(x, ...)
}

#' @rdname dequeS3
#' @return [is.deque()] returns `TRUE` if its argument is a [Deque()]
#' and `FALSE` otherwise.
#' @export
is.deque <- function(x) inherits(x, "Deque")

#' @rdname dequeS3
#' @return `rev` reverses all elements of the deque in place and invisibly
#' returns the [Deque()] object.
#' @export
rev.Deque <- function(x) x$reverse()



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

