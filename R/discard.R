#' Discard Container Elements
#'
#' Search and remove an element from an object. If the element is not found,
#' ignore the attempt.
#' @param .x any `R` object.
#' @param ... elements to be discarded.
#' @export
discard <- function(.x, ...) UseMethod("discard")

#' @rdname discard
#' @export
ref_discard <- function(.x, ...) UseMethod("ref_discard")


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
    (ref_discard(.x$clone(deep = TRUE), ...))
}

#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `discard(.x, ...)` and `ref_discard(.x, ...)` find and discard elements.
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
ref_discard.Container <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    lapply(elems, function(e) .x$discard(e))

    invisible(.x)
}

