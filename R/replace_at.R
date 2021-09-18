#' Strict element replacement at index
#'
#' @description Try to find and replace elements at given indices and signal an
#' error if not found, unless it is stated to explicitly add the element (see
#' option `add`).
#' @param .x any `R` object.
#' @param ... indices at which values are to be replaced.
#' @param .add `logical` if `FALSE` (default) and index is invalid, an error is
#' given. If set to `TRUE` the new element is added at the given index
#' regardless whether the index existed or not. Indices can consist of numbers
#' or names or both, except when adding values at new indices, which is only
#' allowed for names.
#' @details `replace_at` uses copy semantics while `ref_replace_at` works by
#' reference.
#' @export
replace_at <- function(.x, ...) UseMethod("replace_at")

#' @rdname replace_at
#' @export
ref_replace_at <- function(.x, ...) UseMethod("ref_replace_at")


#' @rdname replace_at
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' co = container(a = 0, b = "z")
#' replace_at(co, a = 1)
#' replace_at(co, 1, 1)                 # same
#' replace_at(co, "a", 1)               # same
#'
#' \dontrun{
#' replace_at(co, x = 1)                # names(s) not found: 'x'}
#' replace_at(co, x = 1, .add = TRUE)   # ok (adds x = 1)
#'
#' @export
replace_at.Container <- function(.x, ..., .add = FALSE)
{
    (ref_replace_at(.x$clone(deep = TRUE), ..., .add = .add))
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `replace_at(.x, .., .add = FALSE)` and `ref_replace_at(.x, ..., .add = FALSE)`
#' replace values at given indices. If a given index is invalid, an error is
#' signaled unless `.add` was set to `TRUE`.
#' @examples
#'
#' co = container(a = 0, b = "z")
#' replace_at(co, a = 1)
#' replace_at(co, 1, 1)                 # same
#' replace_at(co, "a", 1)               # same
#'
#' \dontrun{
#' replace_at(co, x = 1)                # names(s) not found: 'x'}
#'
#' replace_at(co, x = 1, .add = TRUE)   # ok (adds x = 1)
NULL


.dissect_and_verify_values = function(values)
{
    indices = names(values)

    hasPair = length(values) == 2 && is.null(names(values))
    if (hasPair) {
        indices = values[[1]]
        values = values[[2]]

        if (length(indices) == 1 && !is.list(indices)) {
            indices = list(indices)
            values = list(values)
        }
    } else {
        verify_names(names(values))
    }

    if (length(indices) > 1 && length(indices) != length(values))
        stop("length of indices (", length(indices),
             ") and values (", length(values), ") don't match", call. = FALSE)

    list(indices = indices, values = values)
}


#' @rdname replace_at
#' @export
ref_replace_at.Container <- function(.x, ..., .add = FALSE)
{
    res = .dissect_and_verify_values(list(...))
    indices = res[["indices"]]
    values = res[["values"]]

    # Verify all numeric indices
    num_indices = as.integer(unlist(Filter(indices, f = is.numeric)))
    stopifnot(.has_valid_num_indices.Container(.x, num_indices))

    if (!isTRUE(.add)) {
        # Also verify all character indices
        char_indices = unlist(Filter(indices, f = is.character))
        stopifnot(.has_valid_char_indices.Container(.x, char_indices))
    }

    replace_or_add_value = function(index, value)
        .x$replace_at(index, value, add = TRUE)

    mapply(replace_or_add_value, indices, values)

    invisible(.x)
}



#' @rdname replace_at
#' @return For `dict.table` an object of class `dict.table`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' replace_at(dit, a = 3:1)
#' replace_at(dit, 1, 3:1)                  # same
#' replace_at(dit, "a", 3:1)                # same
#' replace_at(dit, a = 3:1, b = 6:4)
#' replace_at(dit, 1:2, list(3:1, 6:4))     # same
#'
#' \dontrun{
#' replace_at(dit, x = 1)                   # column(s) not found: 'x'}
#' replace_at(dit, x = 1, .add = TRUE)      # ok (adds column)
replace_at.dict.table <- function(.x, ..., .add = FALSE)
{
    (ref_replace_at(copy(.x), ..., .add = .add))
}

#' @rdname replace_at
#' @export
ref_replace_at.dict.table <- function(.x, ..., .add = FALSE)
{
    res = .dissect_and_verify_values(list(...))
    indices = res[["indices"]]
    values = res[["values"]]

    # Verify all numeric indices
    num_indices = as.integer(unlist(Filter(indices, f = is.numeric)))
    stopifnot(.has_valid_num_indices.dict.table(.x, num_indices))

    if (!isTRUE(.add)) {
        # Also verify all character indices
        char_indices = unlist(Filter(indices, f = is.character))
        stopifnot(.has_valid_char_indices.dict.table(.x, char_indices))
    }

    replace_or_add_column = function(index, value) {
        j <- if (is.numeric(index)) as.integer(index) else index
        data.table::set(.x, j = j, value = value)
    }

    mapply(replace_or_add_column, indices, values)

    invisible(.x)
}

#' @name dict.table
#' @rdname dict.table
#' @details
#' * `replace_at(.x, .., .add = FALSE)` and `ref_replace_at(.x, ..., .add = FALSE)`
#' replace values at given indices. If a given index is invalid, an error is
#' signaled unless `.add` was set to `TRUE`.
#' @examples
#'
#' dit = dict.table(a = 1:3)
#' replace_at(dit, "a", 3:1)
#'
#' \dontrun{
#' replace_at(dit, "b", 4:6)               # column 'b' not in dict.table}
#'
#' replace_at(dit, "b", 4:6, .add = TRUE)  # ok, adds column
NULL

