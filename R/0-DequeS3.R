#' Deque - Double-Ended Queue
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, remove and access elements at both sides of the
#' underlying data sequence. As such, the [deque] can also be used to mimic
#' both stacks and queues.
#' @param ... initial elements put into the `Deque`.
#' @param x `R` object of `ANY` type for [as.deque()] and [is.deque()]
#' or of class `Deque` for the `S3` methods.
#' @name DequeS3
#' @seealso See [container()] for all inherited methods. For the full class
#' documentation see [Deque()] and it's superclass [Container()].
#' @details
#' Methods that alter [Deque] objects usually come in two versions
#' providing either copy or reference semantics where the latter start with
#' `'ref_'` to note the reference semantic, for example, `add()` and
#' `ref_add()`.
#' @examples
#' d = deque(1, 2, s = "a", v = 1:3)
#' is.deque(d)
#' print(d)
#' length(d)
#' names(d)
#' as.list(d)
#' rev(d)
#'
#' l = list(0, 1)
#' d2 = as.deque(l)
#' d + d2
#' c(d, d2) # same as d + d2
#' d2 + d
#' d - d2
#' c(d2, d) # same as d2 + d
#' d2 - d
NULL

#' @rdname DequeS3
#' @details * `deque(...)` initializes and returns an object of class `Deque`
#' @export
deque <- function(...) Deque$new(...)$clone(deep = TRUE)

#' @rdname DequeS3
#' @details * `as.deque(x)` coerces `x` to a deque.
#' @export
as.deque <- function(x) do.call(deque, args = as.list(x))

methods::setOldClass("Deque")
methods::setAs("list", "Deque", function(from) as.deque(from))


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

