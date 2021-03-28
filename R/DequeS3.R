#' Deque (double-ended queue)
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the [deque()] can also be used to mimic
#' both stacks and queues.
#' @param ... initial elements put into the `Deque`.
#' @param x `R` object of `ANY` type for [as.deque()] and [is.deque()]
#' or of class `Deque` for the `S3` methods.
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Deque()] and it's superclass [Container()].
#' @name DequeS3
#' @details While the [Deque()] class is based on the `R6` framework and
#' provides reference semantics, the methods described here provide an `S3`
#' interface with copy semantics. Note that any `S3` methods defined for the
#' `Container` class also work with `Deque` objects.
#' ## Methods
NULL

#' @rdname DequeS3
#' @details * `deque(...)` initializes and returns an object of class `Deque`
#' @export
deque <- function(...) Deque$new(...)$clone(deep = TRUE)

#' @rdname DequeS3
#' @details * `as.deque(x)` coerces `x` to a deque.
#' @export
as.deque <- function(x) do.call(deque, args = as.list(x))


#' @rdname DequeS3
#' @details * `is.deque(x)` returns `TRUE` if `x` is of class `Deque`
#' and `FALSE` otherwise.
#' @export
is.deque <- function(x) inherits(x, "Deque")

#' @export
c.Deque <- function(..., recursive = FALSE, use.names = TRUE)
{
    concat = c.Container(..., recursive = recursive, use.names = use.names)

    if (recursive)
        concat
    else
        as.deque(concat)
}




#' @rdname DequeS3
#' @details * `rev(x)` reverses all elements of the deque in place and invisibly
#' returns the [Deque()] object.
#' @export
rev.Deque <- function(x) x$rev()

