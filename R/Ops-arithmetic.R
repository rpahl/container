#' Arithmetic Operators for Containers
#'
#' @description Binary arithmetic operators for [Container()] objects and
#' derived classes.
#' @name OpsArithmetic
#' @param x,y Depending on the operator at least one must be of class
#' [Container()] or the respective derived class and the other at least be
#' coercible to the respective class.
NULL

#' @rdname OpsArithmetic
#' @return For `Container`, `x + y` combines `x` and `y` into a new container
#' by appending `y` to `x`.
#' @examples
#' c1 = container(1, 1:2)
#' c2 = container(2, 1:2)
#' c1 + c2     # same as c(c1, c2)
#' c2 + c1     # same as c(c2, c1)
#'
#' @export
`+.Container` <- function(x, y)
{
    c(as.container(x), as.container(y))
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @details * `x + y` combines `x` and `y` into a new container by appending `y`
#' to `x`.
#' @examples
#' c1 = container(1, 1:2)
#' c2 = container(2, 1:2)
#' c1 + c2     # same as c(c1, c2)
#' c2 + c1     # same as c(c2, c1)
#'
NULL

#' @rdname OpsArithmetic
#' @return For Container, `x - y` element-wise discards all items of `y`
#' from `x`, given the element was contained in `x`. The result is always a
#' container.
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

#' @name ContainerS3
#' @rdname ContainerS3
#' @details * `x - y` element-wise discards all items of `y` from `x`, given
#' the element was contained in `x`. The result is always a container.
#' @examples
#' c1 - c2
#' c2 - c1
#' c1 - c1
#'
NULL

#' @rdname OpsArithmetic
#' @return For `Deque,` `x + y` combines `x` and `y` into a new deque by
#' appending `y` to `x`.
#' @examples
#' d1 = deque(1, 1:2)
#' d2 = deque(2, 1:2)
#' d1 + d2     # same as c(d1, d2)
#' d2 + d1     # same as c(d2, d1)
#'
#' @export
`+.Deque` <- function(x, y)
{
    c(as.deque(x), as.deque(y))
}

#' @name DequeS3
#' @rdname DequeS3
#' @details * `x + y` combines `x` and `y` into a new deque by appending `y`
#' to `x`.
#' @examples
#' d1 = deque(1, 1:2)
#' d2 = deque(2, 1:2)
#' d1 + d2     # same as c(d1, d2)
#' d2 + d1     # same as c(d2, d1)
#'
NULL

#' @rdname OpsArithmetic
#' @return For Deque, `x - y` element-wise removes all items of `y` from `x`,
#' given the element was contained in `x`.
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

#' @name DequeS3
#' @rdname DequeS3
#' @details * `x - y` element-wise removes all items of `y` from `x`, given
#' the element was contained in `x`.
#' @examples
#' d1 - d2
#' d2 - d1
#' d1 - d1
#'
NULL

#' @rdname OpsArithmetic
#' @return For  `Dict`, `x + y` combines `x` and `y` into a new dict by
#' updating `x` by `y` (see also `[update()]`).
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

#' @name DictS3
#' @rdname DictS3
#' @details * `x + y` combines `x` and `y` into a new dict by updating `x`
#' by `y` (see also `[update()]`).
#' @examples
#' d1 = dict(a = 1, b = list(1, 2))
#' d2 = dict(a = 2, b = list(1, 2))
#' d1 + d2      # same as update(d, d2)
#' d2 + d1      # same as update(d2, d)
#' \dontrun{
#' c(d1, d2)    # duplicated keys are not allowed for Dict}
#'
NULL

#' @rdname OpsArithmetic
#' @return For `Dict`, `x - y` removes all keys from `x` that appear in `y`.
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
        d1$discard_at(key)
    }
    d1
}

#' @name DictS3
#' @rdname DictS3
#' @details * `x - y` removes all keys from `x` that appear in `y`.
#' @examples
#' d - d2
#' d2 - d
#' d - d
#'
NULL


#' @rdname OpsArithmetic
#' @return For `Set`, `x + y` performs the set union.
#' @examples
#' s1 = setnew(1, 1:2)
#' s2 = setnew(2, 1:2)
#' s1 + s2     # same as s1 | s2 or c(c1, s2)
#' s2 + s1     # same
#'
#' @export
`+.Set` <- function(x, y)
{
    x | y
}

#' @name SetS3
#' @rdname SetS3
#' @examples
#' s1 = setnew(1, 1:2)
#' s2 = setnew(2, 1:2)
#' s1 + s2     # same as s1 | s2 or c(c1, s2)
#' s2 + s1     # same
#'
NULL


#' @rdname OpsArithmetic
#' @return For `Set`, `x - y` performs the set difference.
#' @examples
#' s1 - s2
#' s2 - s1
#'
#' @export
`-.Set` <- function(x, y)
{
    as.set(x)$diff(as.set(y))
}

#' @name SetS3
#' @rdname SetS3
#' @examples
#' s1 - s2
#' s2 - s1
#'
NULL
