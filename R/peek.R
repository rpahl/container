#' Peek element of an object
#'
#' Search an element in an object and return a copy of it. If there is no
#' element, return `NULL` or some default value.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
peek <- function(x, ...) UseMethod("peek")

#' @rdname peek
#' @return For `Deque` the last element at the right side of the deque.
#' @export
peek.Deque <- function(x) x$peek()

#' @rdname peek
#' @param key `character` key of the value to peek.
#' @param default `ANY` default value to return if `key` not in the dictionary.
#' @return For `Dict` if `key` does exists, the associated value is returned
#' otherwise the given `default` value.
#' @export
peek.Dict <- function(x, key, default = NULL) x$peek(key, default)

#' @rdname peek
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` returns the column if it does exist otherwise
#' the given `default` value. If the default length does not match the number
#' of rows, it is recycled accordingly and a warning is given, unless the
#' default value had a length of 1.
#' @export
peek.dict.table <- function(x, column, default = NULL)
{
    if (has(x, column)) {
        as.list(x)[[column]]
    } else {
        if (length(default) > 0 && length(default) != nrow(x)) {
            if (length(default) != 1) {
                warning("length of 'default' value did not match number ",
                        "of rows and therefore was recycled")
            }
            default = rep_len(default, length.out = nrow(x))
        }
        default
    }
}



#' Peek element on left side of an object
#'
#' Search an element in an object from left to right and return a copy of the
#' first occurence.  If there is no element, return `NULL` or some default value.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
peekleft <- function(x) UseMethod("peekleft")

#' @rdname peekleft
#' @return For `Deque` the first element at the left side of the deque.
#' @export
peekleft.Deque <- function(x) x$peekleft()



#' Peek random element
#'
#' Randomly select an element from an object and return a copy of it.
#' If there is no element, return `NULL` or some default value.
#' This function can be used to sample randomly (with replacement) from a
#' collection of elements.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
peekitem <- function(x) UseMethod("peekitem")

#' @rdname peekitem
#' @return For `Container` a randomly peeked element is returned.
#' @export
peekitem.Container <- function(x) x$peekitem()

