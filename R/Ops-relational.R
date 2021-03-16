#' Relational Operators for Containers
#'
#' @description Binary comparison operators for [Container()] objects and
#' derived classes.
#' @name OpsComp
#' @param x,y objects of class [Container()] or one of the derived classes.
NULL

#' @export
`==.Container` <- function(x, y) all.equal(x, y)

#' @export
`<.Container` <- function(x, y)
{
    if (length(x) >= length(y))
        return(FALSE)

    x.iter = x$iter()
    y.iter = y$iter()
    while (x.iter$has_next()) {
        xx = x.iter$get_next()
        yy = y.iter$get_next()
        if (!isTRUE(all.equal(xx, yy)))
            return(FALSE)
    }
    TRUE
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

