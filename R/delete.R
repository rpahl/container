#' Delete elements
#'
#' Search and remove an element from an object. If the element is not found,
#' signal an error.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
delete <- function(x, ...) UseMethod("delete")


#' @rdname ContainerS3
#' @param right `logical` if `TRUE`, search from right to left.
#' @param elem some element of any type
#' @details * `delete(x, elem, right)` finds and removes `elem`. If not
#' found, an error is signaled.
#' @export
delete.Container <- function(x, elem, right = FALSE) x$delete(elem, right)


#' @rdname delete
#' @param key `character` key of the value to delete. If `key` does exists,
#' the associated key-value pair is deleted, otherwise an error is signaled.
#' @return For `Dict` the dict object after the key-value pair was removed.
#' @export
delete.Dict <- function(x, key) x$delete(key)


#' @rdname delete
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` the dict.table object after the column was removed.
#' @export
delete.dict.table <- function(x, column)
{
    has_column <- function(column) has(x, column)
    missing_cols = Filter(column, f = Negate(has_column))
    if (length(missing_cols)) {
        col_str = paste0("'", missing_cols, "'", collapse = ", ")
        stop("Column", ifelse(length(missing_cols) > 1, "s ", " "),
             col_str,
             ifelse(is.character(column),
                    paste(" not in", data.class(x)),
                    paste0(" out of range (ncol = ", ncol(x), ")")))
    }
    discard(x, column)
}

