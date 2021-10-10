#' @title Extending base R list and data.table
#'
#' @description
#' container extends the functionality of base R list and the data.table
#' package and with deque, set, and dict provides common data structures
#' not provided by base R, (resembling Python's dict type).
#' In addition, it provides iterators and supports both reference and copy
#' semantics.
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @docType package
#' @name container-pkg
NULL

.onLoad <- function(libname, pkgname)
{
    invisible()
}

