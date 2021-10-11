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
#' is_empty(clear(co))
is_empty.Container <- function(x) x$is_empty()


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `is_empty(x)` `TRUE` if object is empty otherwise `FALSE`
#' @examples
#'
#' co = container(1, 2)
#' is_empty(co)
#' is_empty(clear(co))
NULL


#' @rdname is_empty
#' @export
#' @examples
#'
#' d = dict.table(a = 1:4, b = 4:1)
#' is_empty(d)
#' is_empty(clear(d))
is_empty.dict.table <- function(x) ncol(x) == 0


#' @name dicttable
#' @rdname dicttable
#' @details
#' * `is_empty(x)` `TRUE` if object is empty otherwise `FALSE`
#' @examples
#'
#' d = dict.table(a = 1:4, b = 4:1)
#' is_empty(d)
#' is_empty(clear(d))
NULL
