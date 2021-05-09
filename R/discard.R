#' Discard elements
#'
#' Search and remove an element from an object. If the element is not found,
#' ignore the attempt.
#' @param .x any `R` object.
#' @param ... elements to be discarded. For `Container`, `Deque` and `Set`
#' objects these will be elements contained in the objects. For `Dict` these
#' are key names. For `dict.table` these can be either column names or column
#' indices or both.
#' @export
discard <- function(.x, ...) UseMethod("discard")

#' @rdname discard
#' @export
discard_ <- function(.x, ...) UseMethod("discard_")


#' @rdname discard
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' s = setnew("a", 1:3, iris)
#' print(s)
#' discard(s, 1:3, "a")
#' discard(s, iris)
#' discard(s, "b")  # ignored
#' @export
discard.Container <- function(.x, ...) {
    (discard_(.x$clone(deep = TRUE), ...))
}

#' @name discard.Container
#' @rdname ContainerS3
#' @usage
#' discard(.x, ...)
#' discard_(.x, ...)
#' @details
#' * `discard(.x, ...)` and `discard_(.x, ...)` find and discard elements.
#' Elements that don't exist, are ignored.
#' @examples
#'
#' co = container("a", 1:3, iris)
#' print(co)
#' discard(co, 1:3, "a")
#' discard(co, iris)
#' discard(co, "b")  # ignored
NULL

#' @rdname discard
#' @export
discard_.Container <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    lapply(elems, function(e) .x$discard(e))

    invisible(.x)
}


#' @rdname discard
#' @return For `Dict`, an object of class `Dict`.
#' @examples
#'
#' d = dict(a = 1, b = 2)
#' discard(d, "a", "b")
#' discard(d, c("a", "b"))
#' discard(d, "c")          # ignored
#' @export
discard.Dict <- function(.x, ...) {
    (discard_.Dict(.x$clone(deep = TRUE), ...))
}

#' @name discard.Dict
#' @rdname DictS3
#' @usage
#' discard(.x, ...)
#' discard_(.x, ...)
#' @details
#' * `discard(.x, ...)` and `discard_(.x, ...)` find and remove values at given
#' keys. Keys that don't exist are ignored.
#' @examples
#'
#' d = dict(a = 1, b = 2)
#' discard(d, "a", "b")
#' discard(d, c("a", "b"))
#' discard(d, "c")          # ignored
NULL

#' @rdname discard
#' @export
discard_.Dict <- function(.x, ...)
{
    keys = unlist(list(...))
    if (!length(keys))
        return(.x)

    lapply(keys, function(key) .x$discard(key))

    invisible(.x)
}


#' @rdname discard
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table`, an object of class `dict.table`.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' discard(dit, "ID")
#' discard(dit, "ID", 1)
#' discard(dit, "foo")  # ignored
#' @export
discard.dict.table <- function(.x, ...)
{
    (discard_(clone(.x), ...))
}

#' @name discard.dict.table
#' @rdname dict.table
#' @usage
#' discard(.x, ...)
#' discard_(.x, ...)
#' @details
#' * `discard(.x, ...)` and `discard_(.x, ...)` find and remove columns either by
#' name or index (or both). Columns that don't exist are ignored.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' discard(dit, "ID")
#' discard(dit, "ID", 1)
#' discard(dit, "foo")  # ignored
NULL


#' @rdname discard
#' @export
discard_.dict.table <- function(.x, ...)
{
    args = list(...)
    if (!length(args))
        return(.x)

    # Indices
    indices = as.integer(unlist(Filter(args, f = is.numeric)))
    valid_indices = intersect(indices, seq_len(length(.x)))
    valid_names = names(.x)[valid_indices]

    # Names
    col_names = c(valid_names, unlist(Filter(args, f = is.character)))
    col_names = unique(col_names)
    to_remove = intersect(col_names, colnames(.x))

    if (length(to_remove))
        data.table::set(.x, j = to_remove, value = NULL)

    invisible(.x)
}

