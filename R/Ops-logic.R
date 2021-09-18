#' Logic Operators for Containers
#'
#' @description Binary logic operators for [Container()] objects and
#' derived classes.
#' @name OpsLogic
#' @param x,y Depending on the operator at least one must be of class
#' [Container()] or the respective derived class and the other at least be
#' coercible to the respective class.
NULL


#' @rdname OpsLogic
#' @examples
#' d1 = dict(a = 1, b = 2)
#' d2 = dict(a = 10, x = 4)
#' d1 & d2      # {a = 1}
#'
#' @export
`&.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)
    key_diff <- setdiff(d1$keys(), d2$keys())
    for (key in key_diff) {
        d1$delete_at(key)
    }
    d1
}


#' @name DictS3
#' @rdname DictS3
#' @details * `x` `&` `y` returns a copy of `x` keeping only the keys that
#' are common in both (key intersection), that is, all keys in `x` that do not
#' exist in `y` are removed.
#' @examples
#' d1 = dict(a = 1, b = 2)
#' d2 = dict(a = 10, x = 4)
#' d1 & d2      # {a = 1}
#'
NULL


#' @rdname OpsLogic
#' @export
`|.Dict` <- function(x, y)
{
    d1 = as.dict(x)
    d2 = as.dict(y)
    key_diff <- setdiff(d2$keys(), d1$keys())
    for (key in key_diff) {
        d1$add(key, d2$at2(key))
    }
    d1
}

#' @name DictS3
#' @rdname DictS3
#' @details * `x` `|` `y` returns a copy of `x` extended by all elements of
#' `y` that are stored at keys (or names) that do not exist in `x`, thereby
#' combining the keys of both objects (set union of keys).
#' @examples
#' d1 | d2      # {a = 1, b = 2, x = 4}
#'
NULL


#' @rdname OpsLogic
#' @export
`&.Set` <- function(x, y)
{
    s1 <- as.set(x)
    s2 <- as.set(y)
    s1$intersect(s2)
}

#' @name SetS3
#' @rdname SetS3
#' @details * `x` `&` `y` performs the set intersection of x and y
#' @examples
#' s1 = setnew(1, b = 2)
#' s2 = setnew(1, b = 4)
#' s1 & s2     # {1}
#'
NULL

#' @rdname OpsLogic
#' @export
`|.Set` <- function(x, y)
{
    s1 <- as.set(x)
    s2 <- as.set(y)
    s1$union(s2)
}

#' @name SetS3
#' @rdname SetS3
#' @details * `x` `|` `y` performs the set union of x and y
#' @examples
#' s1 | s2     # {1, b = 2, b = 4}
#'
NULL
