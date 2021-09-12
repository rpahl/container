#' Delete elements at index
#'
#' Search and remove values at given indices, which can be numeric or character
#' or both. If any given index is invalid, an error is signaled. Indices can be
#' numbers or names or both.
#' @param .x any `R` object.
#' @param ... indices at which values are to be deleted.
#' @export
delete_at <- function(.x, ...) UseMethod("delete_at")

#' @rdname delete_at
#' @export
ref_delete_at <- function(.x, ...) UseMethod("ref_delete_at")


.has_valid_num_indices.Container <- function(.x, indices)
{
    bad_indices = setdiff(indices, seq_len(length(.x)))

    allValid = length(bad_indices) == 0
    if (allValid)
        return(TRUE)

    stop("index out of range (length = ", length(.x), "): ", bad_indices[1],
         call. = FALSE)
}

.has_valid_char_indices.Container <- function(.x, indices)
{
    bad_names = setdiff(indices, names(.x))

    allValid = length(bad_names) == 0
    if (allValid)
        return(TRUE)

    stop("names(s) not found: ", paste0("'", bad_names, "'", collapse = ", "),
         call. = FALSE)
}



#' @rdname delete_at
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' co = container(a = 1, b = 2, 3)
#' delete_at(co, "a", "b")          # [3]
#' delete_at(co, 1:2)               # [3]
#' delete_at(co, "a", 3)            # [b = 2]
#' \dontrun{
#' delete_at(co, 4)                 # index out of range
#' delete_at(co, "x")               # names(s) not found: 'x'}
#' @export
delete_at.Container <- function(.x, ...)
{
    (ref_delete_at(.x$clone(deep = TRUE), ...))
}

#' @name ContainerS3methods
#' @rdname ContainerS3
#' @usage
#' delete_at(.x, ...)
#' ref_delete_at(.x, ...)
#' @details
#' * `delete_at(.x, ...)` and `ref_delete_at(.x, ...)` find and remove values at
#' given indices. If any given index is invalid, an error is signaled.
#' @examples
#'
#' co = container(a = 1, b = 2, 3)
#' delete_at(co, "a", "b")          # [3]
#' delete_at(co, 1:2)               # [3]
#' delete_at(co, "a", 3)            # [b = 2]
#' \dontrun{
#' delete_at(co, 4)                 # index out of range
#' delete_at(co, "x")               # names(s) not found: 'x'}
NULL

#' @rdname delete_at
#' @export
ref_delete_at.Container <- function(.x, ...)
{
    indices = list(...)
    if (!length(indices))
        return(.x)

    # Numeric indices
    num_indices = as.integer(unlist(Filter(indices, f = is.numeric)))
    stopifnot(.has_valid_num_indices.Container(.x, num_indices))

    # Character indices
    char_indices = unlist(Filter(indices, f = is.character))
    stopifnot(.has_valid_char_indices.Container(.x, char_indices))

    # Transform all into numeric indices and start deleting from the end
    num_indices = unique(c(num_indices, match(char_indices, names(.x))))
    lapply(sort(num_indices, decreasing = TRUE), function(i) .x$delete_at(i))

    invisible(.x)
}


.has_valid_num_indices.dict.table <- function(.x, indices)
{
    bad_indices = setdiff(indices, seq_len(ncol(.x)))

    allValid = length(bad_indices) == 0
    if (allValid)
        return(TRUE)

    stop("index out of range (ncol = ", ncol(.x), "): ", bad_indices[1],
         call. = FALSE)
}


.has_valid_char_indices.dict.table <- function(.x, col_names)
{
    bad_names = setdiff(col_names, colnames(.x))

    allValid = length(bad_names) == 0
    if (allValid)
        return(TRUE)

    stop("column(s) not found: ",
         paste0("'", bad_names, "'", collapse = ", "),
         call. = FALSE)
}


#' @rdname delete_at
#' @return For `dict.table`, an object of class `dict.table`.
#' @examples
#'
#' dit = as.dict.table(head(sleep))
#' dit
#' delete_at(dit, "ID")
#' delete_at(dit, "ID", 1)
#' \dontrun{
#' delete_at(dit, "foo")   # Column 'foo' not in dict.table}
#' @export
delete_at.dict.table <- function(.x, ...)
{
    (ref_delete_at(clone(.x), ...))
}


#' @name dict.tableMethods
#' @rdname dict.table
#' @usage
#' delete_at(.x, ...)
#' ref_delete_at(.x, ...)
#' @details
#' * `delete_at(.x, ...)` and `ref_delete_at(.x, ...)` find and remove columns either by
#' name or index (or both). If one or more columns don't exist, an error is signaled.
#' @examples
#'
#' (dit = as.dict.table(head(sleep)))
#' delete_at(dit, "ID")
#' delete_at(dit, "ID", 1)
#'
#' \dontrun{
#' delete_at(dit, "foo")   # Column 'foo' not in dict.table}
#'
NULL


#' @rdname delete_at
#' @export
ref_delete_at.dict.table <- function(.x, ...)
{
    args = list(...)
    if (!length(args))
        return(.x)

    # Indices
    indices = as.integer(unlist(Filter(args, f = is.numeric)))
    stopifnot(.has_valid_num_indices.dict.table(.x, indices))

    # Names
    valid_names = names(.x)[indices]
    col_names = c(valid_names, unlist(Filter(args, f = is.character)))
    col_names = unique(col_names)
    stopifnot(.has_valid_char_indices.dict.table(.x, col_names))

    if (length(col_names))
        data.table::set(.x, j = col_names, value = NULL)

    invisible(.x)
}

