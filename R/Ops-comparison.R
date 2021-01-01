#' Relational Operators for Containers
#'
#' @description Binary comparison operators for [Container()] objects and
#' derived classes.
#' @name OpsComp
#' @param x,y objects of class [Container()] or one of the derived classes.
NULL

#' @rdname OpsComp
#' @return For `Set`, `!=` returns `TRUE` if both sets are not equal.
#' @export
`!=.Set` <- function(x, y) !(x$is.equal(y))

#' @rdname OpsComp
#' @return For `Set`, `==` returns `TRUE` if both sets are equal.
#' @export
`==.Set` <- function(x, y) x$is.equal(y)

#' @rdname OpsComp
#' @return For `Set`, `<` returns `TRUE` if x is a *strict* subset of y.
#' @export
`<.Set` <- function(x, y) x$is.subset(y)

#' @rdname OpsComp
#' @return For `Set`, `<=` returns `TRUE` if x is a subset of y.
#' @export
`<=.Set` <- function(x, y) x < y || x == y

#' @rdname OpsComp
#' @return For `Set`, `>` returns `TRUE` if x is a *strict* superset of y.
#' @export
`>.Set` <- function(x, y) x$is.superset(y)

#' @rdname OpsComp
#' @return For `Set`, `>=` returns `TRUE` if x is a subset of y.
#' @export
`>=.Set` <- function(x, y) x > y || x == y

