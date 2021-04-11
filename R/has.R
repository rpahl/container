#' Determine if object has some element
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
has <- function(x, ...) UseMethod("has")


#' @rdname has
#' @param elem some element to be found. For `Container`, `Deque` and
#' `Set` objects, the sought element is passed, while for `Dict` objects this
#' corresponds to the key to be found in the `Dict`.
#' @return `TRUE` if element (or key) is in `x` and otherwise `FALSE`.
#' @export
#' @examples
#'
#' co = container(1, 2, mean)
#' has(co, 1)                   # TRUE
#' has(co, mean)                # TRUE
#' has(co, 1:2)                 # FALSE
has.Container <- function(x, elem) x$has(elem)

#' @name has.Container
#' @rdname ContainerS3
#' @param elem some element of any type
#' @usage
#' has(x, elem)
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
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` `TRUE` if column name or index is in dict.table,
#' otherwise `FALSE`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' has(dit, "a")    # TRUE
#' has(dit, 1)      # TRUE
#' has(dit, "x")    # FALSE
#' has(dit, 3)      # FALSE
has.dict.table <- function(x, column)
{
    if (length(column) != 1)
        stop("column index must be of length 1")

    if (is.na(column))
        stop("undefined column")

    switch(data.class(column),
           "character" = column %in% names(x),
           "numeric" = column > 0 && ncol(x) >= column,
           stop("column must be character or numeric")
    )
}

#' @name has.dict.table
#' @rdname dict.table
#' @usage
#' has(x, column)
#' @details
#' * `has(x, column)` check if some `column` is in dict.table object.
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' has(dit, "a")    # TRUE
#' has(dit, 1)      # TRUE
#' has(dit, "x")    # FALSE
#' has(dit, 3)      # FALSE
NULL

