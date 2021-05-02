#' Strict element access
#'
#' @description Try to access elements and signal an error if not found.
#' @param .x any `R` object.
#' @param ... names or indices to be accessed and additional arguments to be
#' passed to or from methods.
#' @seealso [peek()] for less strict element access.
#' @export
get_at <- function(.x, ...) UseMethod("get_at")


#' @rdname get_at
#' @return For `Dict`, a `Dict` object containing the selected elements.
#' @examples
#'
#' d = dict(a = 1, b = 1:3)
#' get_at(d, "b")
#' get_at(d, "a", "b")      # or alternatively:
#' get_at(d, c("a", "b"))
#' \dontrun{
#' get_at(d, "z")   # key 'z' not in Dict}
#' @export
get_at.Dict <- function(.x, ...)
{
    keys = unlist(list(...))

    d = dict()
    for (k in unique(keys))
        d$add(k, value = .x$at(k))

    d
}


#' @name get_at.Dict
#' @rdname DictS3
#' @usage
#' get_at(.x, ...)
#' @details
#' * `get_at(.x, ...)` retrieves values at given keys. If a `key` does not
#' exist, an error is given.
#' @examples
#'
#' d = dict(a = 1, b = 1:3)
#' get_at(d, "b")
#' get_at(d, "a", "b")      # or alternatively:
#' get_at(dit, c("a", "b"))
#' \dontrun{
#' get_at(d, "z")   # key 'z' not in Dict}
NULL


#' @rdname get_at
#' @return For `dict.table`, a `dict.table` object containing the selected
#' columns.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' get_at(dit, "b")
#' get_at(dit, "a", "b")    # or alternatively:
#' get_at(dit, c("a", "b"))
#' @export
get_at.dict.table <- function(.x, ...)
{
    keys = unlist(list(...))
    has_key = if(is.numeric(keys)) .has_valid_indices else .has_valid_col_names

    d = dict.table()

    if (!length(keys))
        return(d)

    for (k in unique(keys)) {
        stopifnot(has_key(.x, k))

        if (is.numeric(k))
            k = colnames(.x)[k]

        data.table::set(d, j = k, value = .subset2(.x, k))
    }

    d
}


#' @name get_at.dict.table
#' @rdname dict.table
#' @usage
#' get_at(.x, ...)
#' @details
#' * `get_at(.x, ...)` retrieves values at given columns specified either as
#' `numeric` or `character` name. If a column does not exist, an error is given.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' get_at(dit, "b")
#' get_at(dit, "a", "b")    # or alternatively:
#' get_at(dit, c("a", "b"))
NULL

