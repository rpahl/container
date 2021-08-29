#' Comparison Operators for Containers
#'
#' @description Binary comparison operators for [Container()] objects and
#' derived classes.
#' @name ComparisonContainer
#' @param x, y objects of class [Container()] or one of the derived classes.
#' @param x,y at least one must be a [Container()] object (or an object of
#' one of the derived classes) while the other must be at least iterable.
NULL

#' @rdname ContainerS3
#' @details * `x == y` is `TRUE` if the contents of `x` and `y` are
#' lexicographically *equal*.
#' @details * `x != y` is `TRUE` if the contents of `x` and `y` are
#' not equal.
#' @details * `x < y` is `TRUE` if the contents of x are lexicographically
#' *less* than the contents of y.
#' @details * `x <= y` is `TRUE` if the contents of x are lexicographically
#' *less* than or *equal* to the contents of y.
#' @examples
#' c1 = container(1, 2, 3)
#' c2 = container(1, 3, 2)
#' c1 == c1            # TRUE
#' c1 != c2            # TRUE
#' c1 <= c1            # TRUE
#' c1 == c2            # FALSE
#' c1 < c2             # TRUE
#' c1 < container(2)   # TRUE
#' c1 < container()    # FALSE
#'
#' @export
`==.Container` <- function(x, y) isTRUE(all.equal(x, y))


#' @export
`!=.Container` <- function(x, y) !(x == y)


#' @export
`<.Container` <- function(x, y)
{
    if (!is.iterable(x) || !is.iterable(y))
        stop("both arguments must be iterable")

    x.iter = x$iter()
    y.iter = y$iter()

    while (x.iter$has_next()) {
        if (!y.iter$has_next())
            return(FALSE)

        x.elem = x.iter$get_next()[[1]]
        y.elem = y.iter$get_next()[[1]]

        if (y.elem < x.elem)
            return(FALSE)

        if (x.elem < y.elem)
            return(TRUE)
    }
    y.iter$has_next()
}


#' @export
`>.Container` <- function(x, y)
{
    if (!is.iterable(x) || !is.iterable(y))
        stop("both arguments must be iterable")

    y < x
}


#' @export
`<=.Container` <- function(x, y)
{
    if (!is.iterable(x) || !is.iterable(y))
        stop("both arguments must be iterable")

    !(y < x)
}


#' @export
`>=.Container` <- function(x, y)
{
    !(x < y)
}

