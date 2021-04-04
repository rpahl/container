#' @title Container, Deque, Set, and Dict (aka Map) - R6 based container classes
#' with iterators and reference semantics.
#'
#' @description
#' Implements a general Container class with iterators and typical member
#' functions to insert, delete and access objects from the container.
#' The Container class serves as the base class for the Deque, Set and Dict
#' classes (the latter resembling Python's dict type). For any created object
#' both reference semantics or classic S3 copy semantics is provided.
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

