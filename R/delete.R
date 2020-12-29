#' Delete elements
#'
#' Search and remove an element from an object. If the element is not found,
#' signal an error.
#' @param x any `R` object.
#' @param elem element to be deleted from the object. If not found, an
#' error is signaled.
#' @param right `logical` if `TRUE`, search from right to left.
#' @param ... additional arguments to be passed to or from methods.
#' @export
delete <- function(x, ...) UseMethod("delete")

#' @rdname delete
#' @return For `Container` the container object after the element was removed.
#' @export
delete.Container <- function(x, elem, right = FALSE) x$delete(elem, right)

#' @rdname delete
#' @param key `character` key of the value to delete. If `key` does exists,
#' the associated key-value pair is deleted, otherwise an error is signaled.
#' @return For `Dict` the dict object after the key-value pair was removed.
#' @export
delete.Dict <- function(x, key) x$delete(key)

