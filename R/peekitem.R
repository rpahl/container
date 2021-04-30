#' Peek at random item
#'
#' Try to access an element that is chosen randomly from a collection and if
#' not found (i.e. collection is empty), return some default value.
#' The `peekitem` function therefore can be used to sample randomly (with
#' replacement) from a collection of elements.
#' @param .x an `R` object of the respective class.
#' @param default the value that is returned if the intended element does not
#' exist.
#' @param ... additional arguments to be passed to or from methods.
#' @param default value to be returned if collection is empty.
#' @return The value that was randomly chosen from the collection of values.
#' @seealso [peek()], [popitem()]
#' @export
peekitem <- function(.x, ...) UseMethod("peekitem")


#' @rdname peekitem
#' @export
#' @examples
#' co = container(1, 2, 3)
#' peekitem(co)
#' peekitem(container(), default = 9)
peekitem.Container <- function(.x, default = NULL) .x$peekitem(default)


#' @name peekitem.Container
#' @rdname ContainerS3
#' @usage
#' peekitem(.x, default)
#' @details
#' * `peekitem(.x, default = NULL)` peek at random element. If `.x` is empty,
#' return `default`.
#' @examples
#'
#' co = container(1, 2, 3)
#' peekitem(co)
#' peekitem(container(), default = 9)
NULL



#' @rdname peekitem
#' @return For `dict.table`, returns a randomly chosen column. If there are no
#' columns, the `default` value is returned.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:5, b = 5:1)
#' peekitem(dit)
peekitem.dict.table <- function(.x, default = NULL)
{
    if (is_empty(.x))
        return(default)

    key <- sample(names(.x), size = 1)
    peek(.x, key)
}


#' @name peekitem.dict.table
#' @rdname dict.table
#' @usage
#' peekitem(.x, default)
#' @details
#' * `peekitem(.x, default)` return a randomly chosen column. If there are no
#' columns, the `default` value is returned.
#' @examples
#'
#' dit = dict.table(a = 1:5, b = 5:1)
#' peekitem(dit)
NULL

