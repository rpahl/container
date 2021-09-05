#' Check if object is empty
#'
#' @param x any `R` object.
#' @return `TRUE` if object is empty otherwise `FALSE`.
#' @export
is_empty <- function(x) UseMethod("is_empty")

#' @rdname is_empty
#' @export
#' @examples
#'
#' co = container(1, 2)
#' is_empty(co)
#' is_empty(co$clear())
is_empty.Container <- function(x) x$is_empty()


#' @name is_empty.Container
#' @rdname ContainerS3
#' @usage
#' is_empty(x)
#' @details
#' * `is_empty(x)` `TRUE` if object is empty otherwise `FALSE`
#' @export
#' @examples
#'
#' co = container(1, 2)
#' is_empty(co)
#' is_empty(co$clear())
NULL


#' @rdname is_empty
#' @export
#' @examples
#'
#' d = dict.table(a = 1:4, b = 4:1)
#' is_empty(d)
#' is_empty(d$clear())
is_empty.dict.table <- function(x) ncol(x) == 0


#' @name is_empty.dict.table
#' @rdname dict.table
#' @usage
#' is_empty(x)
#' @details
#' * `is_empty(x)` `TRUE` if object is empty otherwise `FALSE`
#' @export
#' @examples
#'
#' d = dict.table(a = 1:4, b = 4:1)
#' is_empty(d)
#' is_empty(d$clear())
NULL
