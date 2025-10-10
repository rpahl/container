#' Add Elements to the Left of Deques
#'
#' Add elements to left side of [Deque] objects.
#' @param .x a `[Deque]` object
#' @param ... elements to be added.
#' @note While [addleft] uses copy semantics [ref_addleft] work by reference.
#' @export
addleft <- function(.x, ...) UseMethod("addleft")

#' @rdname addleft
#' @export
ref_addleft <- function(.x, ...) UseMethod("ref_addleft")


#' @rdname addleft
#' @return For [Deque], an object of class [Deque] with the elements being
#' added to the left of `.x`.
#' @export
#' @examples
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
addleft.Deque <- function(.x, ...)
{
    (ref_addleft(.x$clone(deep = TRUE), ...))
}

#' @name DequeS3
#' @rdname DequeS3
#' @details
#' * `addleft(.x, ...)` adds (possibly named) elements to left side of `.x`.
#' * `ref_addleft(.x, ...)` same as `addleft(.x, ...)` but adds by reference.
#' @examples
#'
#' d = deque(0)
#' add(d, a = 1, b = 2)         # |0, a = 1, b = 2|
#' addleft(d, a = 1, b = 2)     # |b = 2, a = 1, 0|
NULL

#' @rdname addleft
#' @export
ref_addleft.Deque <- function(.x, ...)
{
    elems = list(...)
    elem_names = names(elems)

    for (i in seq_along(elems))
        .x$addleft(elems[[i]], name = elem_names[[i]])

    invisible(.x)
}
