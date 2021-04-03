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
#' @details * `x + y` combines `x` and `y` element-wise into a new container.
#' @export
`+.Container` <- function(x, y)
{
    cx <- as.container(x)
    lapply(as.list(y), cx$add)
    cx
}

#' @rdname ContainerS3
#' @details * `x - y` element-wise removes all items of `y` from `x`, given
#' the element was contained in `x`. The result is always a container.
#' @export
`-.Container` <- function(x, y)
{
    cx <- as.container(x)
    lapply(as.list(y), cx$discard)
    cx
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
    for (key in names(as.list(y)))

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
    s <- as.set(x)
    lapply(as.list(y), s$add)
    s
}

#' @rdname OpsArith
#' @return For `Set`, `-` returns the set-difference of x and y. Result is
#' always a valid set.
#' @export
`-.Set` <- function(x, y)
{
    s <- as.set(x)
    lapply(as.list(y), s$discard)
    s
}


