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
#' @examples
#'
#' s = setnew("a", 1:3, iris)
#' print(s)
#' delete(s, 1:3, "a")
#' delete(s, iris)
#' \dontrun{
#' delete(s, "b")  # "b" is not in Set}
#' @export
delete.Container <- function(.x, ...)
{
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
#' co = container("a", 1:3, iris)
#' print(co)
#' delete(co, 1:3, "a")
#' delete(co, iris)
#' \dontrun{
#' delete(co, "b")   # "b" is not in Container}
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
#' @return For `Dict`, an object of class `Dict`.
#' @examples
#'
#' d = dict(a = 1, b = 2)
#' delete(d, "a", "b")
#' delete(d, c("a", "b"))
#' \dontrun{
#' delete(d, "c")   # key 'c' not in Dict}
#' @export
delete.Dict <- function(.x, ...) {
    (delete_.Dict(.x$clone(deep = TRUE), ...))
}

#' @name delete.Dict
#' @rdname DictS3
#' @usage
#' delete(.x, ...)
#' delete(.x, ...)
#' @details
#' * `delete(.x, ...)` and `delete_(.x, ...)` find and remove values at given
#' keys. Keys that don't exist are ignored.
#' @examples
#'
#' d = dict(a = 1, b = 2)
#' delete(d, "a", "b")
#' delete(d, c("a", "b"))
#' \dontrun{
#' delete(d, "c")   # key 'c' not in Dict}
NULL

#' @rdname delete
#' @export
delete_.Dict <- function(.x, ...)
{
    keys = unlist(list(...))
    if (!length(keys))
        return(.x)

    bad_keys = setdiff(keys, names(.x))
    if (length(bad_keys))
        stop("key(s) not found: ", toString(bad_keys))

    lapply(keys, function(key) .x$delete(key))

    invisible(.x)
}





.has_valid_indices <- function(.x, indices)
{
    bad_indices = setdiff(indices, seq_len(ncol(.x)))

    allValid = length(bad_indices) == 0
    if (allValid)
        return(TRUE)

    stop("index out of range (ncol = ", ncol(.x), "): ", bad_indices)
}


.has_valid_col_names <- function(.x, col_names)
{
    bad_names = setdiff(col_names, colnames(.x))

    allValid = length(bad_names) == 0
    if (allValid)
        return(TRUE)

    stop("column(s) not found: ", toString(bad_names))
}


#' @rdname delete
#' @return For `dict.table`, an object of class `dict.table`.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' dit
#' delete(dit, "ID")
#' delete(dit, "ID", 1)
#' \dontrun{
#' delete(dit, "foo")   # Column 'foo' not in dict.table}
#' @export
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
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' dit
#' delete(dit, "ID")
#' delete(dit, "ID", 1)
#' \dontrun{
#' delete(dit, "foo")   # Column 'foo' not in dict.table}
NULL


#' @rdname delete
#' @export
delete_.dict.table <- function(.x, ...)
{
    args = list(...)
    if (!length(args))
        return(.x)

    # Indices
    indices = as.integer(unlist(Filter(args, f = is.numeric)))
    stopifnot(.has_valid_indices(.x, indices))

    # Names
    valid_names = names(.x)[indices]
    col_names = c(valid_names, unlist(Filter(args, f = is.character)))
    col_names = unique(col_names)
    stopifnot(.has_valid_col_names(.x, col_names))

    if (length(col_names))
        data.table::set(.x, j = col_names, value = NULL)

    invisible(.x)
}

