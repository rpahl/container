#' Pop element
#'
#' Search and return an element and remove it afterwards from the object.
#' If the element is not found, signal an error.
#' @name pop
#' @details
#' All functions work by reference, that is, the original object is altered.
#' `pop_(.x)` tries to access specific values.
#'
#' `popleft_(.x)` pops first element of a `Deque`.
#'
#' @param .x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @seealso [peek()]
#' @export
pop_ <- function(.x, ...) UseMethod("pop_")

#' @rdname pop
#' @export
popleft_ <- function(.x, ...) UseMethod("popleft_")

#' @rdname pop
#' @return For `Deque` the first (`popleft_`) or last (`pop_`) element of the
#' deque after it was removed.
#' @export
#' @examples
#' # Deque
#' d = deque(1, 2, 3)
#' pop_(d)
#' popleft_(d)
#'
#' \dontrun{
#' pop_(deque())  # pop at empty Deque}
pop_.Deque <- function(.x) .x$pop()

#' @rdname pop
#' @export
popleft_.Deque <- function(.x) .x$popleft()

#' @name pop.Deque
#' @rdname DequeS3
#' @usage
#' pop_(.x)
#' popleft_(.x)
#' @details
#' * `pop_(.x)` pop last element. If `.x` is empty, an error is given.
#' * `popleft_(.x)` pop first element. If `.x` is empty, an error is given.
#' @examples
#' d = deque(1, 2, 3)
#' pop_(d)
#' popleft_(d)
#'
#' \dontrun{
#' pop_(deque())  # pop at empty Deque}
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
#' pop_(d, "b")
#' print(d)
#'
#' \dontrun{
#' pop_(d, "x")  # key 'x' not in Dict}
pop_.Dict <- function(.x, key) .x$pop(key)

#' @rdname pop
#' @return For `dict.table`, returns the column named `key` after it was
#' removed from the dict.table. If column does not exist, an error is raised.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' pop_(dit, "a")
#' print(dit)
#' \dontrun{
#' pop_(dit, "x")  # Column 'x' not in dict.table}
pop_.dict.table <- function(.x, key)
{
    elem <- peek(.x, key)
    delete_(.x, key)
    elem
}


