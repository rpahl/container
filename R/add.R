#' Add element
#'
#' @param x any `R` object.
#' @param elem some element to be added.
#' @param ... additional arguments to be passed to or from methods.
#' @export
add <- function(x, ...) UseMethod("add")

#' @rdname add
#' @return For `Container`, the container object with the element being added.
#' @export
add.Container <- function(x, elem) x$add(elem)

#' @rdname add
#' @param key `character` unique identifier of the item.
#' @return For `Dict`, the dict object with the key-element pair being added.
#' @export
add.Dict <- function(x, key, elem) x$add(key, elem)


#' Add element to the left
#'
#' @param x any `R` object.
#' @param elem some element to be added.
#' @param ... additional arguments to be passed to or from methods.
#' @export
addleft <- function(x, ...) UseMethod("addleft")

#' @rdname addleft
#' @return For `Deque`, the deque object with the element being added to the
#' left.
#' @export
addleft.Deque <- function(x, elem) x$addleft(elem)

