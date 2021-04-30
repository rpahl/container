#' Pop element
#'
#' Search and return an element and remove it afterwards from the object.
#' If the element is not found, signal an error.
#' The `popitem` function can be used to sample randomly (without replacement)
#' from a collection of elements and this way to destructively iterate over
#' collections as often used in set algorithms.
#' @details
#' `pop` tries to access specific values.
#'
#' `popleft` pops first element of a `Deque`.
#'
#' `popitem` randomly pops an element from the object.
#'
#' @param .x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @seealso [peek()], [peekitem()]
#' @export
pop <- function(.x, ...) UseMethod("pop")

#' @rdname pop
#' @export
popleft <- function(.x, ...) UseMethod("popleft")

#' @rdname pop
#' @export
popitem <- function(.x, ...) UseMethod("popitem")


#' @rdname pop
#' @return For `Deque` the first (`popleft`) or last (`pop`) element of the
#' deque after it was removed.
#' @export
#' @examples
#' # Deque
#' d = deque(1, 2, 3)
#' pop(d)
#' popleft(d)
#'
#' \dontrun{
#' pop(deque())  # pop at empty Deque}
pop.Deque <- function(.x) .x$pop()

#' @rdname pop
#' @export
popleft.Deque <- function(.x) .x$popleft()

#' @name pop.Deque
#' @rdname DequeS3
#' @usage
#' pop(.x)
#' popleft(.x)
#' @details
#' * `pop(.x)` pop last element. If `.x` is empty, an error is given.
#' * `popleft(.x)` pop first element. If `.x` is empty, an error is given.
#' @examples
#' d = deque(1, 2, 3)
#' pop(d)
#' popleft(d)
#'
#' \dontrun{
#' pop(deque())  # pop at empty Deque}
NULL


#' @rdname pop
#' @param key `character` name of the value to pop.
#' the associated key-value pair is deleted and it's value returned.
#' @return For `Dict` the value associated with the key after the key-value
#' pair was removed from the dict. If `key` is not found, an error is raised.
#' @export
#' @examples
#'
#' # Dict
#' d = dict(a = 1, b = 1:3)
#' pop(d, "b")
#' print(d)
#'
#' \dontrun{
#' pop(d, "x")  # key 'x' not in Dict}
pop.Dict <- function(.x, key) .x$pop(key)

#' @rdname pop
#' @return For `dict.table`, returns the column named `key` after it was
#' removed from the dict.table. If column does not exist, an error is raised.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' pop(dit, "a")
#' print(dit)
#' \dontrun{
#' pop(dit, "x")  # Column 'x' not in dict.table}
pop.dict.table <- function(.x, column)
{
    elem <- peek(.x, column)
    delete_(.x, column)
    elem
}


