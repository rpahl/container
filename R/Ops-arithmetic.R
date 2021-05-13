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
#' @examples
#' c1 = container(1, 1:2)
#' c2 = container(2, 1:2)
#' c1 + c2     # same as c(c1, c2)
#' c2 + c1     # same as c(c2, c1)
#' @export
`+.Container` <- function(x, y)
{
    c(as.container(x), as.container(y))
}

#' @rdname ContainerS3
#' @details * `x - y` element-wise discards all items of `y` from `x`, given
#' the element was contained in `x`. The result is always a container.
#' @examples
#' c1 - c2
#' c2 - c1
#' c1 - c1
#'
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
#' @examples
#' d1 = deque(1, 1:2)
#' d2 = deque(2, 1:2)
#' d1 + d2     # same as c(d1, d2)
#' d2 + d1     # same as c(d2, d1)
#' @export
`+.Deque` <- function(x, y)
{
    c(as.deque(x), as.deque(y))
}


#' @rdname OpsArith
#' @details * `x - y` element-wise removes all items of `y` from `x`, given
#' the element was contained in `x`.
#' @return For `Deque`, `-` returns x - y
#' @examples
#' d1 - d2
#' d2 - d1
#' d1 - d1
#'
#' @export
`-.Deque` <- function(x, y)
{
    deq <- as.deque(x)
    lapply(as.list(y), deq$discard)
    deq
}


#' @rdname OpsArith
#' @return For `Dict`, `+` returns a copy of `x` updated by `y`.
#' @examples
#' d1 = dict(a = 1, b = list(1, 2))
#' d2 = dict(a = 2, b = list(1, 2))
#' d1 + d2      # same as update(d, d2)
#' d2 + d1      # same as update(d2, d)
#' \dontrun{
#' c(d1, d2)    # duplicated keys are not allowed for Dict}
#'
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
#' @examples
#' d - d2
#' d2 - d
#' d - d
#'
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


