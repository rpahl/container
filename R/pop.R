#' Pop element
#'
#' Search and remove an element from an object and return a copy of the
#' element. If the element is not found, signal an error.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
pop <- function(x, ...) UseMethod("pop")

#' @rdname pop
#' @return For `Deque` the element from the right side of the deque after it
#' was removed.
#' @export
pop.Deque <- function(x) x$pop()

#' @rdname pop
#' @param key `character` key of the value to pop. If `key` does exists,
#' the associated key-value pair is deleted and it's value returned.
#' @return For `Dict` the value associated with the key after the key-value
#' pair was removed from the dict.
#' @export
pop.Dict <- function(x, key) x$pop(key)

#' @rdname pop
#' @param column `character` name or `numeric` index of column.
#' @return For `dict.table` the column after it was removed from the dict.table.
#' @export
pop.dict.table <- function(x, column)
{
    elem <- peek(x, column)
    delete(x, column)
    elem
}


#' Pop element from left
#'
#' Search an element in an object from left to right and for the first
#' occurence return a copy and remove it.
#' If the element is not found, signal an error.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
popleft <- function(x) UseMethod("popleft")

#' @rdname popleft
#' @return For `Deque` the first element at the left side of the deque after it
#' was removed.
#' @export
popleft.Deque <- function(x) x$popleft()



#' Pop random element
#'
#' Randomly select and remove an element from an object and return a copy of the
#' element. If there is no element at all, signal an error.
#' This function can be used to destructively iterate over a collection of
#' elements as often used in set algorithms.
#' @param x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @export
popitem <- function(x) UseMethod("popitem")

#' @rdname popitem
#' @return For `Container` a randomly popped element is returned.
#' @export
popitem.Container <- function(x) x$popitem()

#' @rdname popitem
#' @export
popitem.dict.table <- function(x)
{
    if (empty(x)) {
        stop("popitem at empty ", data.class(x))
    }
    column <- sample(names(x), 1)
    pop(x, column)
}

