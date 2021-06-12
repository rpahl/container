#' Extract or Replace Part of a Container Object
#'
#' @description Extract or replace parts of a `Container` object similar
#' to R's base extract operators on lists.
#' @name OpsExtract
#' @param x `Container` object for which to extract or replace elements.
#' @param j `numeric` or `character` indices to extract.
#' @param name a literal character string or a name (possibly backtick quoted).
#' @return the values at the given indices.
NULL

#' @export
`[.Container` <- function(x, j)
{
    peek_at(x, j)
}


#' @export
`[[.Container` <- function(x, j)
{
    peek_at2(x, j)
}


#`$.Container` <- function(x, name)
#{
#    as.list(x)$name
#}

#' @export
"$<-.Container" = function(x, name, value)
{
    x = clone(x)
    x
}

