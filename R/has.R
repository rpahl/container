#' Check for Element
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @seealso [has_name()]
#' @export
has <- function(x, ...) UseMethod("has")


#' @rdname has
#' @param elem some element to be found.
#' @return `TRUE` if element is in `x` and otherwise `FALSE`.
#' @export
#' @examples
#'
#' co = container(1, 2, mean)
#' has(co, 1)                   # TRUE
#' has(co, mean)                # TRUE
#' has(co, 1:2)                 # FALSE
has.Container <- function(x, elem, ...) x$has(elem)

#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `has(x, elem)` `TRUE` if element is in `x` and otherwise `FALSE`.
#' @examples
#'
#' co = container(1, 2, mean)
#' has(co, 1)                   # TRUE
#' has(co, mean)                # TRUE
#' has(co, 1:2)                 # FALSE
NULL


#' @rdname has
#' @param column vector of values with the same length as the number of rows
#' of the `dict.table`.
#' @return For `dict.table`, `TRUE` if column exists in `x` otherwise `FALSE`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:3, b = as.list(4:6))
#' has(dit, 1:3)            # TRUE
#' has(dit, 4:6)            # FALSE
#' has(dit, as.list(4:6))   # TRUE
has.dict.table <- function(x, column, ...)
{
    if (!length(x))
        return(FALSE)

    if (length(column) != nrow(x)) {
        warning("length of column vector (", length(column), ") ",
                "does not match number of rows (", nrow(x), ")", call. = FALSE)
        return(FALSE)
    }

    has(as.container(x), column)
}


#' @name dicttable
#' @rdname dicttable
#' @details
#' * `has(x, column)` check if some `column` is in dict.table object.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = as.list(4:6))
#' has(dit, 1:3)            # TRUE
#' has(dit, 4:6)            # FALSE
#' has(dit, as.list(4:6))   # TRUE
NULL

