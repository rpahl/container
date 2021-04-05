#' @title Container, Deque, Set, and Dict (aka Map) - R6 based container classes
#' with iterators and reference semantics.
#'
#' @description
#' Common container data structures deque, set, dict (resembling
#' Python's dict type) and dict.table (combining dict and data.table) with
#' typical member functions to insert, delete and access container elements.
#' Furthermore, iterators are provided and for any created object both
#' reference and copy semantics are supported simultaneously.
#'
#' @author Roman Pahl, \email{roman.pahl@gmail.com}
#' @docType package
#' @name container-pkg
NULL


.ask_data.table_install = function()
{
    question = "Do you want to install the 'data.table' package now?"
    choice = utils::menu(c("Yes", "No"), title = question)

    if (choice == 1)
        install.packages("data.table")

    invisible()
}


.onLoad <- function(libname, pkgname)
{
    invisible()
}

