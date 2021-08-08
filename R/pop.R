#' Pop element
#'
#' Search and return an element and remove it afterwards from the object.
#' If the element is not found, signal an error.
#' @name pop
#' @details
#' All functions work by reference, that is, the original object is altered.
#' `ref_pop(.x)` tries to access specific values.
#'
#' `ref_poplefref_popleft(.x)` pops first element of a `Deque`.
#'
#' @param .x any `R` object.
#' @param ... additional arguments to be passed to or from methods.
#' @seealso [peek()]
#' @export
ref_pop <- function(.x, ...) UseMethod("ref_pop")

#' @rdname pop
#' @export
ref_popleft <- function(.x, ...) UseMethod("ref_popleft")

#' @rdname pop
#' @return For `Deque` the first (`ref_popleft`) or last (`ref_pop`) element of
#' the deque after it was removed.
#' @export
#' @examples
#' # Deque
#' d = deque(1, 2, 3)
#' ref_pop(d)
#' ref_popleft(d)
#'
#' \dontrun{
#' ref_pop(deque())  # pop at empty Deque}
ref_pop.Deque <- function(.x) .x$pop()

#' @rdname pop
#' @export
ref_popleft.Deque <- function(.x) .x$popleft()

#' @name pop.Deque
#' @rdname DequeS3
#' @usage
#' ref_pop(.x)
#' ref_popleft(.x)
#' @details
#' * `ref_pop(.x)` pop last element. If `.x` is empty, an error is given.
#' * `ref_popleft(.x)` pop first element. If `.x` is empty, an error is given.
#' @examples
#' d = deque(1, 2, 3)
#' ref_pop(d)
#' print(d)
#' ref_popleft(d)
#' print(d)
#'
#' \dontrun{
#' ref_pop(deque())  # pop at empty Deque}
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
#' ref_pop(d, "b")
#' print(d)
#'
#' \dontrun{
#' ref_pop(d, "x")  # key 'x' not in Dict}
ref_pop.Dict <- function(.x, key) .x$pop(key)

#' @rdname pop
#' @return For `dict.table`, returns the column named `key` after it was
#' removed from the dict.table. If column does not exist, an error is raised.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' ref_pop(dit, "a")
#' print(dit)
#' \dontrun{
#' ref_pop(dit, "x")  # Column 'x' not in dict.table}
ref_pop.dict.table <- function(.x, key)
{
    elem <- at2(.x, key)
    ref_delete_at.dict.table(.x, key)
    elem
}


