#' Extract or Replace Parts of a Container Object
#'
#' @description Extract or replace parts of a `Container` object similar
#' to R's base extract operators on lists.
#' @name ExtractContainer
#' @param x `Container` object for which to extract or replace elements.
#' @param i,...  indices specifying elements to extract or replace. Indices
#' are `numeric` or `character` vectors or a `list` containing both.
#' @param name `character` string (possibly backtick quoted)
#' @param value the replacing value of `ANY` type
NULL

#' @rdname ExtractContainer
#' @export
#' @usage
#' x[i]
#' x[[i]]
#' x[...]
`[.Container` <- function(x, ...)
{
    dots = tryCatch(list(...), error = identity)
    if (inherits(dots, "error"))
        return(x)

    peek_at(x, ...)
}


#' @export
`[[.Container` <- function(x, i)
{
    x$peek_at2(i)
}


#' @rdname ExtractContainer
#' @export
#' @usage
#' x[i] <- value
#' x[[i]] <- value
#' x$name <- value
`[<-.Container` = function(x, name, value)
{
    #clone(x)$replace_at(name, value, add = TRUE)
}

#' @export
`[[<-.Container` = function(x, name, value)
{
    #clone(x)$replace_at(name, value, add = TRUE)
}

#' @export
`$<-.Container` = function(x, name, value)
{
    clone(x)$replace_at(name, value, add = TRUE)
}


