#' Delete elements
#'
#' Search and remove elements from an object. If the element is not found,
#' an error is signaled.
#' @param .x any `R` object.
#' @param ... elements to be deleted. For `Container`, `Deque` and `Set`
#' objects these will be elements contained in the objects. For `Dict` these
#' are key names. For `dict.table` these can be either column names or column
#' indices or both.
#' @export
delete <- function(.x, ...) UseMethod("delete")

#' @rdname delete
#' @export
delete_ <- function(.x, ...) UseMethod("delete_")


#' @rdname delete
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @export
#' @examples
#'
#' s = setnew("a", 1:3, iris)
#' print(s)
#' delete(s, 1:3, "a")
#' delete(s, iris)
#' \dontrun{
#' delete(s, "b")  # "b" is not in Set}
#' discard(s, "b")  # ok - command is ignored
delete.Container <- function(.x, ...) {
    (delete_(.x$clone(deep = TRUE), ...))
}

#' @name delete.Container
#' @rdname ContainerS3
#' @usage
#' delete(.x, ...)
#' delete_(.x, ...)
#' @details
#' * `delete(.x, ...)` and `delete_(.x, ...)` find and remove elements.
#' If one or more elements don't exist, an error is signaled.
#' @examples
#'
#' s = setnew("a", 1:3, iris)
#' print(s)
#' delete(s, 1:3, "a")
#' delete(s, iris)
#' \dontrun{
#' delete(s, "b")   # "b" is not in Set}
#' discard(s, "b")  # ok - command is ignored
NULL

#' @rdname delete
#' @export
delete_.Container <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    hasElements = sapply(elems, function(e) .x$has(e))

    if (any(!hasElements)) {
        # Throw error by trying to delete first missing element
        element = elems[!hasElements][[1]]
        .x$delete(element)
    }

    lapply(elems, function(e) .x$delete(e))
    invisible(.x)
}



#' @rdname delete
#' @return For `dict.table`, an object of class `dict.table`.
#' @export
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' dit
#' delete(dit, "ID")
#' delete(dit, "ID", 1)
#' \dontrun{
#' delete(dit, "foo")   # Column 'foo' not in dict.table}
#' discard(dit, "foo")  # ok - command is ignored
delete.dict.table <- function(.x, ...)
{
    (delete_(clone(.x), ...))
}


#' @name delete.dict.table
#' @rdname dict.table
#' @usage
#' delete(.x, ...)
#' delete_(.x, ...)
#' @details
#' * `delete(.x, ...)` and `delete_(.x, ...)` find and remove columns either by
#' name or index (or both). If one or more columns don't exist, an error is
#' signaled.
#' @export
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' dit
#' delete(dit, "ID")
#' delete(dit, "ID", 1)
#' \dontrun{
#' delete(dit, "foo")   # Column 'foo' not in dict.table}
#' discard(dit, "foo")  # ok - command is ignored
delete_.dict.table <- function(.x, ...)
{
    columns = list(...)
    if (!length(columns))
        return(.x)

    hasColumns = sapply(columns, function(column) has(.x, column))

    if (any(!hasColumns)) {
        firstMissing = columns[!hasColumns][[1]]

        stop("Column '", firstMissing, "'",
             ifelse(is.character(firstMissing),
                    paste(" not in", data.class(.x)),
                    paste0(" out of range (ncol = ", ncol(.x), ")")))
    }

    discard_(.x, ...)
}

