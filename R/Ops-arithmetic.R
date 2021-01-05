#' Arithmetic Operators for Containers
#'
#' @description Binary arithmetic operators for [Container()] objects and
#' derived classes.
#' @name OpsArith
#' @param x,y Depending on the operator at least one or both must be of
#' class [Container()] or the respective derived class. Both must be at least
#' iterable.
NULL

#' @rdname OpsArith
#' @return For `Set`, `|` returns the union of both sets.
#' @export
`|.Set` <- function(x, y) x$union(y)

#' @rdname OpsArith
#' @return For `Set` , `&` returns the intersection of both sets.
#' @export
`&.Set` <- function(x, y) x$intersect(y)

#' @rdname OpsArith
#' @return For `Set`, `-` returns the set-difference of both sets.
#' @export
`-.Set` <- function(x, y) x$diff(y)


#' @rdname OpsArith
#' @return For `Container`, `|` returns a new [Container()] object containing
#' all elements from `x` and `y`. Note that x | y and y | x will yield identical
#' results.
#' @export
`|.Container` <- function(x, y)
{
    if (is.container(x)) {
        co <- x$clone()
        it <- iter(y)
    } else {
        return(y | x)
    }
    while(has_next(it)) {
        co$add(get_next(it))
    }
    co
}


#' @rdname OpsArith
#' @return For `Deque`, `|` returns a new [Deque()] object with all elements
#' from `x` and `y`. If `x` is not a [Deque()], it's elements will be added to
#' the left of `y`.
#' @export
`|.Deque` <- function(x, y)
{
    if (is.deque(x)) {
        deq <- x$clone()
        it <- iter(y)
        while(has_next(it)) {
            deq$add(get_next(it))
        }
    } else {
        deq <- y$clone()
        it <- iter(x)
        while(has_next(it)) {
            deq$addleft(get_next(it))
        }
    }
    deq
}


#' @rdname OpsArith
#' @return For `Dict`, `+` returns a copy of `x` updated by `y`.
#' @export
`+.Dict` <- function(x, y)
{
    if (!(is.dict(x) && is.dict(y))) {
        stop("both arguments must be dicts")
    }
    x$clone(deep = TRUE)$update(y)
}


#' @rdname OpsArith
#' @return For `Dict`, `-` returns a copy of `x` with all `y` keys being
#' discarded, that is, it keeps only those keys of `x` that are not in `y`.
#' @export
`-.Dict` <- function(x, y)
{
    if (!(is.dict(x) && is.dict(y))) {
        stop("both arguments must be dicts")
    }
    xx <- x$clone(deep = TRUE)
    for (key in y$keys()) {
        xx$discard(key)
    }
    xx
}

#' @rdname OpsArith
#' @return For `Dict`, `&` returns a copy of `x` keeping only the keys that
#' are common in both (key intersection), that is, all keys in `x` that do not
#' exist in `y` were removed.
#' @export
`&.Dict` <- function(x, y)
{
    if (!(is.dict(x) && is.dict(y))) {
        stop("both arguments must be dicts")
    }
    common_keys <- intersect(x$keys(), y$keys())
    d <- dict()
    for (key in common_keys) {
        d$add(key, x$getval(key))
    }
    d
}

