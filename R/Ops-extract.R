#' Extract or Replace Part of a Container Object
#'
#' @description Extract or replace parts of a `Container` object similar
#' to R's base extract operators on lists.
#' @name OpsExtract
#' @param x `Container` object for which to extract or replace elements.
#' @param j `numeric` or `character` indices to extract.
#' @param ... indices of elements to be extracted
#' @param name a literal character string or a name (possibly backtick quoted).
#' @return the values at the given indices.
NULL

#' @export
`[.Container` <- function(x, ...)
{
    dots = tryCatch(list(...), error = identity)
    if (inherits(dots, "error"))
        return(x)

    peek_at(x, ...)
}


#' @export
`[[.Container` <- function(x, j)
{
    x$peek_at2(j)
}


#' @export
`$<-.Container` = function(x, name, value)
{
    x = clone(x)
    x
}

