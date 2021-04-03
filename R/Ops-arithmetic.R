#' Arithmetic Operators for Containers
#'
#' @description Binary arithmetic operators for [Container()] objects and
#' derived classes.
#' @name OpsArith
#' @param x,y Depending on the operator at least one must be of class
#' [Container()] or the respective derived class and the other at least be
#' coercible to the respective class.
NULL

#' @rdname ContainerS3
#' @details * `x + y` combines `x` and `y` into a new container by appending `y`
#' to `x`.
#' @export
`+.Container` <- function(x, y)
{
    c(as.container(x), as.container(y))
}

#' @rdname ContainerS3
#' @details * `x - y` element-wise discards all items of `y` from `x`, given
#' the element was contained in `x`. The result is always a container.
#' @export
`-.Container` <- function(x, y)
{
    co <- as.container(x)
    lapply(as.list(y), co$discard)
    co

}


#' @rdname OpsArith
#' @return For `Deque`, `+` returns a new [Deque()] object with all elements
#' from `x` and `y` combined. If `x` is not a [Deque()], it's elements will
#' be added to the left of `y`.
#' @export
`+.Deque` <- function(x, y)
{
    c(as.deque(x), as.deque(y))
}


#' @rdname OpsArith
#' @details * `x - y` element-wise removes all items of `y` from `x`, given
#' the element was contained in `x`.
#' @return For `Deque`, `-` returns x - y
#' @export
`-.Deque` <- function(x, y)
{
    deq <- as.deque(x)
    lapply(as.list(y), deq$discard)
    deq
}


#' @rdname OpsArith
#' @return For `Dict`, `+` returns a copy of `x` updated by `y`.
#' @export
`+.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)
    d1$update(d2)
}


#' @rdname OpsArith
#' @return For `Dict`, `-` returns `x` after all keys were discarded
#' that appear in `y`.
#' @export
`-.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)

    for (key in d2$keys()) {
        d1$discard(key)
    }
    d1
}

#' @rdname OpsArith
#' @return For `Set`, `+` returns the set union of x and y. Result is always a
#' valid set.
#' @export
`+.Set` <- function(x, y)
{
    x | y
}

#' @rdname OpsArith
#' @return For `Set`, `-` returns the set-difference of x and y. Result is
#' always a valid set.
#' @export
`-.Set` <- function(x, y)
{
    as.set(x)$diff(as.set(y))
}


