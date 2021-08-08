#' Discard elements at index
#'
#' Search and remove values at given indices, which can be numeric or character
#' or both. Invalid indices are ignored.
#' @param .x any `R` object.
#' @param ... indices at which values are to be discarded.
#' @export
discard_at <- function(.x, ...) UseMethod("discard_at")

#' @rdname discard_at
#' @export
ref_discard_at <- function(.x, ...) UseMethod("ref_discard_at")


#' @rdname discard_at
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' co = container(a = 1, b = 2, 3)
#' discard_at(co, "a", "b")         # [3]
#' discard_at(co, 1:2)              # [3]
#' discard_at(co, "a", 3)           # [b = 2]
#' discard_at(co, "x")              # ignored
#' @export
discard_at.Container <- function(.x, ...) {
    (ref_discard_at(.x$clone(deep = TRUE), ...))
}

#' @name discard.Container
#' @rdname ContainerS3
#' @usage
#' discard_at(.x, ...)
#' ref_discard_at(.x, ...)
#' @details
#' * `discard_at(.x, ...)` and `ref_discard_at(.x, ...)` find and discard values
#' at given indices. Invalid indices are ignored.
#' @examples
#'
#' co = container(a = 1, b = 2, 3)
#' discard_at(co, "a", "b")         # [3]
#' discard_at(co, 1:2)              # [3]
#' discard_at(co, "a", 3)           # [b = 2]
#' discard_at(co, "x")              # ignored
NULL

#' @rdname discard_at
#' @export
ref_discard_at.Container <- function(.x, ...)
{
    indices = list(...)
    if (!length(indices))
        return(.x)

    # Numeric indices
    num_indices = as.integer(unlist(Filter(indices, f = is.numeric)))

    # Character indices
    char_indices = unlist(Filter(indices, f = is.character))

    # Transform all into numeric indices and start discarding from the end
    num_indices = unique(c(num_indices, match(char_indices, names(.x))))
    valid_indices = intersect(num_indices, seq_len(length(.x)))
    lapply(sort(valid_indices, decreasing = TRUE), function(i) .x$discard_at(i))

    invisible(.x)
}



#' @rdname discard_at
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table`, an object of class `dict.table`.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' discard_at(dit, "ID")
#' discard_at(dit, "ID", 1)
#' discard_at(dit, "foo")  # ignored
#' @export
discard_at.dict.table <- function(.x, ...)
{
    (ref_discard_at(clone(.x), ...))
}

#' @name discard.dict.table
#' @rdname dict.table
#' @usage
#' discard_at(.x, ...)
#' ref_discard_at(.x, ...)
#' @details
#' * `discard_at(.x, ...)` and `ref_discard_at(.x, ...)` find and remove columns
#' either by name or index (or both). Invalid column indices are ignored.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' discard_at(dit, "ID")
#' discard_at(dit, "ID", 1)
#' discard_at(dit, "foo")  # ignored
NULL


#' @rdname discard_at
#' @export
ref_discard_at.dict.table <- function(.x, ...)
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

