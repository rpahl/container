#' Strict element replacement
#'
#' @description Try to find and replace elements and signal an error if not
#' found, unless it is stated to explicitly add the element (see option `add`).
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @param add `logical` if FALSE (default) and element (or key) was not found,
#' an error is given. In contrast, if set to `TRUE` the new element is added
#' regardless of whether it is used as a replacement for an existing element or
#' just added as a new element.
#' @details `replace` uses copy semantics while `replace_` works by reference.
#' @export
replace <- function(x, ...) UseMethod("replace")

#' @rdname replace
#' @export
replace_ <- function(x, ...) UseMethod("replace_")

#' @export
replace.default <- function(x, ...)
{
    base::replace(x, ...)
}


#' @rdname replace
#' @param old old element to be found and replaced.
#' @param new the new element replacing the old one.
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # just add the zero without replacement
#'
#' @export
replace.Container <- function(x, old, new, add = FALSE)
{
    replace_(x$clone(deep = TRUE), old, new, add)
}

#' @name replace.Container
#' @rdname ContainerS3
#' @usage
#' replace(x, old, new, add = FALSE)
#' replace_(x, old, new, add = FALSE)
#' @details
#' * `replace(x, old, new, add = FALSE)` and `replace_(x, ...)` try to find
#' element `old` and replace it with element `new`. If `old` does not exist,
#' an error is raised, unless `add` was set to `TRUE`.
#' @examples
#' co = container("x", 9)
#' replace(co, 9, 0)
#' replace(co, "x", 0)
#' \dontrun{
#' replace(co, "z", 0)              # old element ("z") is not in Container}
#' replace(co, "z", 0, add = TRUE)  # ok, adds the element
#'
NULL

#' @rdname replace
#' @export
replace_.Container <- function(x, old, new, add = FALSE)
{
    x$replace(old, new, add = add)
}



#' @rdname replace
#' @param key `character` name of key. For `dict.table` the `key` can also be a
#' numeric index.
#' @return For `Dict` an object of class `Dict`.
#' @export
#' @examples
#' d = dict(a = 1)
#' replace(d, "a", 1:5)
#' \dontrun{
#' replace(d, "b", 2)              # key 'b' not in Dict}
#' replace(d, "b", 2, add = TRUE)  # ok, adds value
replace.Dict <- function(x, key, value, add = FALSE)
{
    replace_(x$clone(deep = TRUE), key, value, add)
}

#' @rdname replace
#' @export
replace_.Dict <- function(x, key, value, add = FALSE)
{
    x$replace(key, value, add)
}


#' @name replace.Dict
#' @rdname DictS3
#' @usage
#' replace(x, key, value, add = FALSE)
#' replace_(x, key, value, add = FALSE)
#' @details
#' * `replace(x, key, value, add = FALSE)` and `replace_(x, ...)` replace value
#' at `key`. If `key` does not exist, an error is given unless `add` was set to
#' `TRUE`.
#' @examples
#' d = dict(a = 1)
#' replace(d, "a", 1:5)
#' \dontrun{
#' replace(d, "b", 2)              # key 'b' not in Dict}
#' replace(d, "b", 2, add = TRUE)  # ok, adds value
NULL


#' @rdname replace
#' @param key `character` name or `numeric` index of column.
#' @return For `dict.table` an object of class `dict.table`.
#' @export
#' @examples
#' dit = dict.table(a = 1:3)
#' replace(dit, "a", 3:1)
#' \dontrun{
#' replace(dit, "b", 4:6)               # column 'b' not in dict.table}
#' replace(dit, "b", 4:6, add = TRUE)   # ok, adds value
replace.dict.table <- function(x, key, value, add = FALSE)
{
    replace_(copy(x), key, value, add)
}

#' @rdname replace
#' @export
replace_.dict.table <- function(x, key, value, add = FALSE)
{
    if (!add && !has(x, key)) {
        if (is.character(key))
            stop("column '", key, "' not in ", data.class(x), ". ",
                 "To add the column, use 'add = TRUE'.")
        else
            stop(key, " is outside range [1, ncol = ", ncol(x), "]")
    }

    j <- if (is.numeric(key)) as.integer(key) else key
    data.table::set(x, j = j, value = value)

    x
}

#' @name replace.dict.table
#' @rdname dict.table
#' @usage
#' replace(x, key, value, add = FALSE)
#' replace_(x, key, value, add = FALSE)
#' @details
#' * `replace(x, key, value, add = FALSE)` and `replace_(x, ...)` replace
#' values at column `key`. If `key` does not exist, an error is given unless
#' `add` was set to `TRUE`.
#' @examples
#' dit = dict.table(a = 1:3)
#' replace(dit, "a", 3:1)
#' \dontrun{
#' replace(dit, "b", 4:6)               # column 'b' not in dict.table}
#' replace(dit, "b", 4:6, add = TRUE)   # ok, adds value
NULL

