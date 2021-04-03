#' @rdname Ops
#' @return For `Dict`, `&` returns a copy of `x` keeping only the keys that
#' are common in both (key intersection), that is, all keys in `x` that do not
#' exist in `y` were removed.
#' @export
`&.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)
    key_diff <- setdiff(d1$keys(), d2$keys())
    for (key in key_diff) {
        d1$delete(key)
    }
    d1
}

#' @rdname Ops
#' @return For `Dict`, `|` returns a copy of `x` extended by all elements of
#' `y` that are stored at keys (or names) that do not exist in `x`, thereby
#' combining the keys of both objects (i.e. basically union of keys).
#' @export
`|.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)
    key_diff <- setdiff(d2$keys(), d1$keys())
    for (key in key_diff) {
        d1$add(key, d2$get(key))
    }
    d1
}


#' @rdname Ops
#' @return For `Set`, `&` returns the set intersection of x and y
#' @export
`&.Set` <- function(x, y)
{
    s1 <- as.set(x)
    s2 <- as.set(y)
    s1$intersect(s2)
}

#' @rdname Ops
#' @return For `Set`, `|` returns the set union of x and y
#' @export
`|.Set` <- function(x, y)
{
    s1 <- as.set(x)
    s2 <- as.set(y)
    s1$union(s2)
}

