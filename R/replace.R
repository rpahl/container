#' Strict element replacement
#'
#' @description Try to find and replace elements and signal an error if not
#' found, unless it is stated to explicitly add the element (see option `add`).
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param add `logical` if FALSE (default) and element was not found, an error is
#' given. In contrast, if set to `TRUE` the new element is added regardless
#' whether it already exists or not, that is, either as a replacement or just
#' as a new element.
#' @param .copy `logical` if TRUE (default) the replacement operation is done on
#' a copy of `x`, otherwise on the original object.
#' @export
replace <- function(x, ...) UseMethod("replace")


#' @export
replace.default <- function(x, ...)
{
    base::replace(x, ...)
}


#' @rdname replace
#' @param old old element to be found and replaced.
#' @param new the new element replacing the old one.
#' @return For `Container`, an object of class `Container` (or of the
#' respective derived class) with the element being replaced (or added).
#' @examples
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # just add the zero without replacement
#'
#' @export
replace.Container <- function(x, old, new, add = FALSE, .copy = TRUE)
{
    co = if (.copy)
        x$clone(deep = TRUE)
    else
        x

    co$replace(old, new, add = add)
}


#' @name replace.Container
#' @rdname ContainerS3
#' @usage ## S3 method for class 'Container'
#' replace(x, old, new, add = FALSE)
#' @details * `replace(x, old, new, add = FALSE)` tries to find element `old`
#' and replace it with element `new`. If `old` does not exist, an error is
#' raised, unless `add` was set to `FALSE`.
#' @examples
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # just add the zero without replacement
#'
NULL


#' @rdname replace
#' @param key `character` name of key.
#' @return For `Dict`, and object of class `Dict` with `value` replaced at
#' `key` if `key` is already in the `Dict`. If `key` is not in the `Dict`,
#' an error is given unless `add` was set to `TRUE` in which case the `value`
#' is added under `key`.
#' @export
replace.Dict <- function(x, key, value, add = FALSE, .copy = TRUE)
{
    d = if (.copy)
        x$clone(deep = TRUE)
    else
        x

    d$replace(key, value, add)
}


#' @rdname replace
#' @param key `character` name or `numeric` index of column.
#' @return For `dict.table`, an object of class `dict.table` with values
#' replaced at column `key`, which can be a name or a numeric index.
#' If column `key` does not exist, an error is given
#' unless `add` was set to `TRUE` in which case the column is added if possible.
#' All changes are made by reference.
#' @export
replace.dict.table <- function(x, key, value, add = FALSE, .copy = TRUE)
{
    if (!add && !has(x, key)) {
        if (is.character(key))
            stop("column '", key, "' not in ", data.class(x), ". ",
                 "To add the column, use 'add = TRUE'.")
        else
            stop(key, " is outside range [1, ncol = ", ncol(x), "]")
    }

    dit = as.dict.table(x, copy = .copy)
    j <- if (is.numeric(key)) as.integer(key) else key
    data.table::set(dit, j = j, value = value)

    dit
}

