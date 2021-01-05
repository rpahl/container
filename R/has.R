#' Determine if object has some element
#'
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
has <- function(x, ...) UseMethod("has")


#' @rdname ContainerS3
#' @param elem some element of any type
#' @details * `has(x)` `TRUE` if element is in `x` and otherwise `FALSE`.
#' @export
has.Container <- function(x, elem) x$has(elem)


#' @rdname has
#' @param key `character` name of key to search for.
#' @return For `Dict` `TRUE` if key is in dict otherwise `FALSE`.
#' @export
has.Dict <- function(x, key) x$has(key)


#' @rdname has
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` `TRUE` if column name or index is in dict.table,
#' otherwise `FALSE`.
#' @export
has.dict.table <- function(x, column)
{
    if (length(column) != 1) stop("column index must be of length 1")
    if (is.na(column)) stop("undefined column")
    switch(data.class(column),
           "character" = column %in% names(x),
           "numeric" = ncol(x) >= column,
           stop("column must be character or numeric")
    )
}

