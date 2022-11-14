#' Extract Single Elements Safely
#'
#' Extracts the value of a Container at the given index. If the index is
#' invalid, an error is signaled. If given as a string, the element matching the
#' name is returned. If there are two or more identical names, the value of the
#' first match (i.e. *leftmost* element) is returned.
#' Extract value at index. If index is invalid or not found, an error is
#' signaled. If given as a string, the element matching the name is returned.
#' If there are two or more identical names, the value of the first match (i.e.
#' *leftmost* element) is returned.
#' @param x an `R` object of the respective class.
#' @param index `character` name or `numeric` position of the sought value.
#' @param ... other arguments passed to or from methods.
#' @seealso [peek_at2()] for less strict extraction
#' @export
at2 <- function(x, ...) UseMethod("at2")


#' @rdname at2
#' @return For `Container`, returns the value at the given index.
#' @export
#' @examples
#'
#' # Container
#' co = container(a = 1, 2, b = 3, 4)
#' at2(co, 1)
#' at2(co, "a")
#' at2(co, 2)
#' try(at2(co, "x"))     # index 'x' not found
#' try(at2(co, 5))       # index 5 exceeds length of Container
#'
#' # Dict
#' d = dict(a = 1, b = 3)
#' at2(d, 1)
#' at2(d, "a")
#' at2(d, 2)
#' try(at2(d, "x"))     # index 'x' not found
#' try(at2(d, 5))       # index 5 exceeds length of Dict
#' @export
at2.Container <- function(x, index, ...)
{
    x$at2(index)
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `at2(x, index)` returns the value at the given index or signals an error
#' if not found.
#' @examples
#'
#' co = container(a = 1, 2, b = 3, 4)
#' at2(co, 1)
#' at2(co, "a")
#' at2(co, 2)
#' try(at2(co, "x"))     # index 'x' not found
#' try(at2(co, 5))       # index 5 exceeds length of Container
NULL


#' @rdname at2
#' @return For `dict.table`, returns the column at the given `index` or signals
#' an error if not found.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' at2(dit, 1)
#' at2(dit, "a")
#' at2(dit, 2)
#' try(at2(dit, "x"))     # index 'x' not found
#' try(at2(dit, 5))       # index 5 exceeds length of dict.table
at2.dict.table <- function(x, index, ...)
{
    .assert_index_and_arg(x, index)
    .subset2(x, index)
}


#' @name dict.table
#' @rdname dict.table
#' @details
#' * `at2(x, index)` returns the column at the given `index` or signals
#' an error if not found.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' at2(dit, 1)
#' at2(dit, "a")
#' at2(dit, 2)
#' try(at2(dit, "x"))     # index 'x' not found
#' try(at2(dit, 5))       # index 5 exceeds length of dict.table
NULL


