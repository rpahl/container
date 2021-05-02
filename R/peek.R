#' Peek at element
#'
#' Try to access element and return some default value if not found.
#' The `peek` function basically enables safe element access in cases where
#' elements don't exist or the accessed objects is empty, thereby freeing the
#' user from additional effort (like if-statements) that would be required to
#' catch these cases manually.
#' @details
#' `peek` tries to access specific values.
#'
#' `peekleft` peeks at first element of a `Deque`.
#' @param x an `R` object of the respective class.
#' @param default the value that is returned if the intended element does not
#' exist.
#' @param ... additional arguments to be passed to or from methods.
#' @param default value to be returned if peeked value does not exist.
#' @seealso [pop()]
#' @export
peek <- function(x, ...) UseMethod("peek")

#' @rdname peek
#' @export
peekleft <- function(x, ...) UseMethod("peekleft")


#' @rdname peek
#' @return For `Deque` the first (`peekleft`) or last (`peek`) element.
#' @export
#' @examples
#' # Deque
#' d = deque(1, 2, 3)
#' peek(d)
#' peekleft(d)
#' peek(deque())
#' peek(deque(), default = 0)
#' peekleft(deque(), default = 0)
peek.Deque <- function(x, default = NULL) x$peek(default)

#' @rdname peek
#' @export
peekleft.Deque <- function(x, default = NULL) x$peekleft(default)


#' @name peek.Deque
#' @rdname DequeS3
#' @usage
#' peek(x, default)
#' peekleft(x, default)
#' @details
#' * `peek(x, default = NULL)` peek at last element. If `x` is empty, return
#' `default`.
#' * `peekleft(x, default = NULL)` peek at first element. If `x` is empty,
#' return `default`.
#' @examples
#'
#' d = deque(1, 2, 3)
#' peek(d)
#' peekleft(d)
#' peek(deque())
#' peek(deque(), default = 0)
#' peekleft(deque(), default = 0)
NULL


#' @rdname peek
#' @param key `character` name of the value to peek.
#' @return For `Dict`, returns the associated value if `key` does exists,
#' otherwise the given `default` value.
#' @export
#' @examples
#'
#' # Dict
#' d = dict(a = 1, b = 1:3)
#' peek(d, "b")
#' peek(d, "x")
#' peek(d, "x", default = 4:7)
peek.Dict <- function(x, key, default = NULL)
{
    x$peek(key, default)
}

#' @name peek.Dict
#' @rdname DictS3
#' @usage
#' peek(x, key, default)
#' @details
#' * `peek(x, key, default)` returns the associated value if `key` does exists,
#' otherwise the given `default` value.
#' @examples
#'
#' d = dict(a = 1, b = 1:3)
#' peek(d, "b")
#' peek(d, "x")
#' peek(d, "x", default = 4:7)
NULL


#' @rdname peek
#' @return For `dict.table`, returns the column named `key` if it exist otherwise
#' the given `default` value. If the default length does not match the number
#' of rows, it is recycled accordingly and a warning is given, unless the
#' default value has a length of 1, in which case recycling is done silently.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek(dit, "a")
#' peek(dit, 1)
#' peek(dit, 3)
#' peek(dit, "x")
#' peek(dit, "x", default = 0)
peek.dict.table <- function(x, key, default = NULL)
{
    if (has(x, key))
        return(.subset2(x, key))

    if (length(default) && length(default) != nrow(x)) {
        if (length(default) != 1)
            warning("length of 'default' value (", length(default), ") ",
                    "did not match number of rows (", nrow(x), ") ",
                    "and therefore was recycled")

        default = rep_len(default, length.out = nrow(x))
    }
    default
}


#' @name peek.dict.table
#' @rdname dict.table
#' @usage
#' peek(x, key, default)
#' @details
#' * `peek(x, key, default)` return column named `key` if it exist otherwise
#' the given `default` value. If the default length does not match the number
#' of rows, it is recycled accordingly and a warning is given, unless the
#' default value has a length of 1, in which case recycling is done silently.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek(dit, "a")
#' peek(dit, 1)
#' peek(dit, 3)
#' peek(dit, "x")
#' peek(dit, "x", default = 0)
NULL



