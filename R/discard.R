#' Discard elements
#'
#' Search and remove an element from an object. If the element is not found,
#' ignore the attempt.
#' @param .x any `R` object.
#' @param ... elements to be discarded.
#' @export
discard <- function(.x, ...) UseMethod("discard")

#' @rdname discard
#' @export
discard_ <- function(.x, ...) UseMethod("discard_")


#' @rdname discard
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' s = setnew("a", num = 1:3, data = iris)
#' print(s)
#' discard(s, 1:3, "a")
#' discard(s, iris)
#' discard(s, "b")  # ignored
#' @export
discard.Container <- function(.x, ...) {
    (discard_(.x$clone(deep = TRUE), ...))
}

#' @name discard.Container
#' @rdname ContainerS3
#' @usage
#' discard(.x, ...)
#' discard_(.x, ...)
#' @details
#' * `discard(.x, ...)` and `discard_(.x, ...)` find and discard elements.
#' Elements that don't exist, are ignored.
#' @examples
#'
#' co = container("a", num = 1:3, data = iris)
#' print(co)
#' discard(co, 1:3, "a")
#' discard(co, iris)
#' discard(co, "b")  # ignored
NULL

#' @rdname discard
#' @export
discard_.Container <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    lapply(elems, function(e) .x$discard(e))

    invisible(.x)
}

