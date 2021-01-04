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
    if (length(x) == 0) return(deque())
    UseMethod("as.deque")
}

#' @export
as.deque.default <- function(x)
{
    do.call(deque, args = as.list(x))
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
rev.Deque <- function(x) x$rev()


