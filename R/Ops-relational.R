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
    y < x
}


#' @export
`<=.Container` <- function(x, y)
{
    !(y < x)
}


#' @export
`>=.Container` <- function(x, y)
{
    !(x < y)
}



#' @rdname OpsComp
#' @return For `Set`, `!=` returns `TRUE` if both sets are not equal.
#' @export
`!=.Set` <- function(x, y) !(x$is_equal(y))

#' @rdname OpsComp
#' @return For `Set`, `==` returns `TRUE` if both sets are equal.
#' @export
`==.Set` <- function(x, y) `==.Container`(x, y)

#' @rdname OpsComp
#' @return For `Set`, `<` returns `TRUE` if x is a *proper* subset of y.
#' @export
`<.Set` <- function(x, y) `<.Container`(x, y)

#' @rdname OpsComp
#' @return For `Set`, `<=` returns `TRUE` if x is a subset of y.
#' @export
`<=.Set` <- function(x, y) x$is_subset(y)

#' @rdname OpsComp
#' @return For `Set`, `>` returns `TRUE` if x is a *proper* superset of y.
#' @export
`>.Set` <- function(x, y) y$is_proper_subset(x)

#' @rdname OpsComp
#' @return For `Set`, `>=` returns `TRUE` if x is a superset of y.
#' @export
`>=.Set` <- function(x, y) y$is_subset(x)

