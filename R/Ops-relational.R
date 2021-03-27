#' Relational Operators for Containers
#'
#' @description Binary comparison operators for [Container()] objects and
#' derived classes.
#' @name OpsComp
#' @param x,y objects of class [Container()] or one of the derived classes.
NULL

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

