#' Deque (double-ended queue)
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the [deque()] can also be used to mimic
#' both stacks and queues.
#' @details The [Deque()] class inherits from class [Container()] and extends
#' it by `pop` and `peek` methods, element counting, and reverse and rotate
#' functionality. For documentation of inherited methods, see [container()].
#'
#' Deque methods:
#' @param ... initial elements put into the `Deque`.
#' @param keep_names `logical` TRUE keeps names of passed elements.
#' @param elem an `R` object of any type
#' @param x any `R` object for [as.deque()] and [is.deque()]. An
#' object of class `Deque` for the `S3` methods.
#' @seealso See [container()] for all inherited methods. For the class
#' documentation see [Deque()] and it's superclass [Container()].
#' @name dequeS3
NULL


#' @rdname dequeS3
#' @details * `deque(...)` initializes and returns an object of class `Deque`
#' @export
deque <- function(..., keep_names = FALSE) {
    if (missing(keep_names)) {
        Deque$new(...)
    } else {
        Deque$new(..., keep_names = keep_names)
    }
}


#' @rdname dequeS3
#' @details * `as.deque(x)` coerces `x` to a deque.
#' @export
as.deque <- function(x)
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
#' @details * `is.deque(x)` returns `TRUE` if `x` is of class `Deque`
#' and `FALSE` otherwise.
#' @export
is.deque <- function(x) inherits(x, "Deque")

#' @rdname dequeS3
#' @details * `addleft(x, elem)` adds element `elem` to the left side of
#' deque `x`.
#' @export
addleft <- function(x, elem) x$addleft(elem)


#' @rdname dequeS3
#' @details * `rev(x)` reverses all elements of the deque in place and invisibly
#' returns the [Deque()] object.
#' @export
rev.Deque <- function(x) x$rev()

