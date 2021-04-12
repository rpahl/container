#' Discard elements
#'
#' Search and remove an element from an object. If the element is not found,
#' ignore the attempt.
#' @param x any `R` object.
#' @param ... elements to be discarded. For `Container`, `Deque` and `Set`
#' objects these will be elements contained in the objects. For `Dict` these
#' are key names. For `dict.table` these can be either column names or column
#' indices or both.
#' @export
discard <- function(x, ...) UseMethod("discard")

#' @rdname discard
#' @export
discard_ <- function(x, ...) UseMethod("discard_")

#' @rdname discard
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @export
discard.Container <- function(x, ...) {
    (discard_(x$clone(deep = TRUE), ...))
}

#' @name discard.Container
#' @rdname ContainerS3
#' @usage
#' discard(x, ...)
#' discard_(x, ...)
#' @details
#' * `discard(x, ...)` and `discard_(x, ...)` find and discard elements.
#' Elements that don't exist, are ignored.
NULL


#' @rdname discard
#' @export
discard_.Container <- function(x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(x)

    lapply(elems, function(e) x$discard(e))

    invisible(x)
}

#' @rdname discard
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table`, an object of class `dict.table`.
#' @export
discard.dict.table <- function(x, ...)
{
    (discard_(clone(x), ...))
}


#' @name discard.dict.table
#' @rdname dict.table
#' @usage
#' discard(x, ...)
#' discard_(x, ...)
#' @details
#' * `discard(x, ...)` and `discard_(x, ...)` find and remove columns either by
#' name or index (or both). If one or more columns don't exist, an error is
#' signaled.
#' @export
discard_.dict.table <- function(x, ...)
{
    columns = list(...)
    if (!length(columns))
        return(x)


    indices = as.integer(Filter(columns, f = is.numeric))
    indices = Filter(indices, f = function(i) i <= ncol(x))

    tab_names = as.character(Filter(columns, f = is.character))
    tab_names = c(tab_names, names(x)[indices])

    to_remove = intersect(tab_names, colnames(x))

    if (length(to_remove))
        data.table::set(x, j = to_remove, value = NULL)

    invisible(x)
}

