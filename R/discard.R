#' Discard elements
#'
#' Search and remove an element from an object. If the element is not found, do
#' nothing.
#' @param x any `R` object.
#' @param elem element to be deleted from the object. If elem is not found, the
#' object is not altered.
#' @param right `logical` if `TRUE`, search from right to left.
#' @param ... additional arguments to be passed to or from methods.
#' @export
discard <- function(x, ...) UseMethod("discard")

#' @rdname discard
#' @return For `Container` the container object after the element was removed
#' or the unchanged container object if the element was not contained in the
#' container in the first place.
#' @export
discard.Container <- function(x, elem, right = FALSE) x$discard(elem, right)

#' @rdname discard
#' @param key `character` key of value to discard. If `key` does exist,
#' the associated key-value pair is deleted, otherwise it is ignored.
#' @return For `Dict` the dict object after the key-value pair was removed or
#' the unchanged dict object if the key was not in the dict in the first place.
#' @export
discard.Dict <- function(x, key) x$discard(key)

#' @rdname discard
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` the dict.table object after the column was removed
#' or the unchanged dict.table object if the column did not exist in the first
#' place.
#' @export
discard.dict.table <- function(x, column)
{
    j = Filter(unique(column), f = function(column) has(x, column))
    if (is.numeric(j)) j = as.integer(j)

    if (length(j)) {
        data.table::set(x, j = j, value = NULL)
    }
    invisible(x)
}

