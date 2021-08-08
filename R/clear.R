#' Clear a container
#'
#' Removes all elements from the container object.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
clear <- function(x, ...) UseMethod("clear")

#' @rdname clear
#' @export
ref_clear <- function(x, ...) UseMethod("ref_clear")


#' @rdname clear
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @export
#' @examples
#'
#' co = container(1, 2, mean)
#' clear(co)
#' co
#' ref_clear(co)
#' co
clear.Container <- function(x) (x$clone(deep = TRUE)$clear())

#' @name clear.Container
#' @rdname ContainerS3
#' @usage
#' clear(x)
#' ref_clear(x)
#' @details
#' * `clear(x)` and `ref_clear(x)` remove all elements from `x`.
#' @examples
#'
#' co = container(1, 2, mean)
#' clear(co)
#' print(co)    # Original was not touched
#' ref_clear(co)   # Clears original
#' print(co)
NULL

#' @rdname clear
#' @export
ref_clear.Container <- function(x)
{
    invisible(x$clear())
}


#' @rdname clear
#' @return For `dict.table` an object of class `dict.table`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' clear(dit)
#' dit
#' ref_clear(dit)
#' dit
clear.dict.table <- function(x) dict.table()

#' @name clear.dict.table
#' @rdname dict.table
#' @usage
#' clear(x)
#' ref_clear(x)
#' @details
#' * `clear(x)` and ref_clear(x) remove all elements from `x`.
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' clear(dit)
#' dit
#' ref_clear(dit)
#' dit
NULL

#' @rdname clear
#' @export
ref_clear.dict.table <- function(x)
{
    data.table::set(x, j = seq_len(ncol(x)), value = NULL)

    invisible(x)
}

