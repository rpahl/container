#' Pop a random item
#'
#' Randomy select and return an element and remove it afterwards. If there is
#' no such element, signal an error.
#' The `popitem` function can be used to sample randomly (without replacement)
#' from a collection of elements and this way to destructively iterate over
#' collections as often used in set algorithms.
#' @param x an `R` object of the respective class.
#' @param ... additional arguments to be passed to or from methods.
#' @return The value that was randomly chosen from the collection of values.
#' @seealso [pop()], [peekitem()]
#' @export
popitem <- function(x, ...) UseMethod("popitem")


#' @rdname popitem
#' @export
#' @examples
#' co = container(1, 2, 3)
#' while (!is_empty(co))
#'     print(popitem(co))
popitem.Container <- function(x) x$popitem()


#' @name popitem.Container
#' @rdname ContainerS3
#' @usage
#' popitem(x)
#' @details
#' * `popitem(x)` pop random element from `x`. If `x` is empty, an error is
#' signaled.
#' @examples
#'
#' co = container(1, 2, 3)
#' while (!is_empty(co))
#'     print(popitem(co))
NULL



#' @rdname popitem
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 3:1)
#' while (!is_empty(dit))
#'     print(popitem(dit))
popitem.dict.table <- function(x)
{
    if (is_empty(x)) {
        stop("popitem at empty ", data.class(x))
    }
    column <- sample(names(x), 1)
    pop(x, column)
}


#' @name popitem.dict.table
#' @rdname dict.table
#' @usage
#' popitem(x)
#' @details
#' * `popitem(x)` return a randomly chosen column. If there are no
#' columns, an error is signaled.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 3:1)
#' while (!is_empty(dit))
#'     print(popitem(dit))
NULL

