#' Pop element
#'
#' Search and return an element and remove it afterwards from the object.
#' If the element is not found, signal an error.
#' @name pop
#' @details
#' All functions work by reference, that is, the original object is altered.
#' `ref_pop(.x)` tries to access specific values.
#'
#' `ref_popleft(.x)` pops first element of a `Deque`.
#'
#' @param .x any `R` object.
#' @param index `character` name or `numeric` position of value to be popped
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
#' @return For `Container` the value at the given index after it was removed from
#' the `Container` object. If `index` is not found, an error is raised.
#' @export
#' @examples
#'
#' # Container
#' co = container(a = 1, b = 1:3, d = "foo")
#' ref_pop(co, "b")
#' ref_pop(co, 1)
#'
#' \dontrun{
#' ref_pop(co, "x")  # index 'x' not found}
#'
ref_pop.Container <- function(.x, index) .x$pop(index)

#' @name pop.Container
#' @rdname ContainerS3
#' @usage
#' ref_pop(.x, index)
#' @details
#' * `ref_pop(.x, index)` return element at given index and remove it
#' from the `container` object.
#' @examples
#'
#' co = container(a = 1, b = 1:3, d = "foo")
#' ref_pop(co, "b")
#' ref_pop(co, 1)
#'
#' \dontrun{
#' ref_pop(co, "x")  # index 'x' not found}
#'
NULL


#' @rdname pop
#' @return For `dict.table`, returns the column at the given `index` after it was
#' removed from the `dict.table`. If column does not exist, an error is raised.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' ref_pop(dit, "a")
#' ref_pop(dit, 1)
#'
#' \dontrun{
#' ref_pop(dit, "x")  # index 'x' not found}
#'
ref_pop.dict.table <- function(.x, index)
{
    elem <- at2(.x, index)
    ref_delete_at.dict.table(.x, index)
    elem
}


#' @name pop.dict.table
#' @rdname dict.table
#' @usage
#' ref_pop(.x, index)
#' @details
#' * `ref_pop(.x, index)` return element at given column index and remove the
#' column from the dict.table object.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' ref_pop(dit, "a")
#' ref_pop(dit, 1)
#'
#' \dontrun{
#' ref_pop(dit, "x")  # index 'x' not found}
#'
NULL
