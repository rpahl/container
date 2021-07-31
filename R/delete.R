#' Delete elements
#'
#' Search and remove elements from an object. If the element is not found,
#' an error is signaled.
#' @param .x any `R` object.
#' @param ... elements to be deleted.
#' @export
delete <- function(.x, ...) UseMethod("delete")

#' @rdname delete
#' @export
delete_ <- function(.x, ...) UseMethod("delete_")


#' @rdname delete
#' @return For `Container`, an object of class `Container` (or one of the
#' respective derived classes).
#' @examples
#'
#' s = setnew("a", 1:3, iris)
#' print(s)
#' delete(s, 1:3, "a")
#' delete(s, iris)
#' \dontrun{
#' delete(s, "b")  # "b" is not in Set}
#' @export
delete.Container <- function(.x, ...)
{
    (delete_(.x$clone(deep = TRUE), ...))
}

#' @name delete.Container
#' @rdname ContainerS3
#' @usage
#' delete(.x, ...)
#' delete_(.x, ...)
#' @details
#' * `delete(.x, ...)` and `delete_(.x, ...)` find and remove elements.
#' If one or more elements don't exist, an error is signaled.
#' @examples
#'
#' co = container("a", 1:3, iris)
#' print(co)
#' delete(co, 1:3, "a")
#' delete(co, iris)
#' \dontrun{
#' delete(co, "b")   # "b" is not in Container}
NULL

#' @rdname delete
#' @export
delete_.Container <- function(.x, ...)
{
    elems = list(...)
    if (!length(elems))
        return(.x)

    hasElements = sapply(elems, function(e) .x$has(e))

    if (any(!hasElements)) {
        # Throw error by trying to delete first missing element
        element = elems[!hasElements][[1]]
        .x$delete(element)
    }

    lapply(elems, function(e) .x$delete(e))
    invisible(.x)
}


