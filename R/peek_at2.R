#' Peek at one element
#'
#' Try to access element and return some default value if not found.
#' In contrast to `[at2()]`, this function provides a less stricter element
#' access, that is, it remains valid even if peeked elements don't exist.
#' @details
#' `peek_at2` tries to access specific values.
#'
#' `peek` peek at last element of a `Deque`.
#'
#' `peekleft` peek at first element of a `Deque`.
#' @param x an `R` object of the respective class.
#' @param index `character` name or `numeric` position of the sought value.
#' @param default value to be returned if peeked value does not exist.
#' @seealso [at2()] for strict element extraction
#' @export
peek_at2 <- function(x, index, default = NULL) UseMethod("peek_at2")

#' @rdname peek_at2
#' @export
peekleft <- function(x, default = NULL) UseMethod("peekleft")

#' @rdname peek_at2
#' @export
peek <- function(x, default = NULL) UseMethod("peek")


#' @rdname peek_at2
#' @return For `Container`, returns the value at the given index or (if not
#' found) the given default value.
#' @export
#' @examples
#'
#' # Container
#' co = container(a = 1, 2, b = 3, 4)
#' peek_at2(co, 1)
#' peek_at2(co, "a")
#' peek_at2(co, "x")
#' peek_at2(co, "x", default = 0)
#'
#' # Dict
#' d = dict(a = 1, b = 1:3)
#' peek_at2(d, "b")
#' peek_at2(d, "x")
#' peek_at2(d, "x", default = 4:7)
peek_at2.Container <- function(x, index, default = NULL)
{
    x$peek_at2(index, default)
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `peek_at2(x, index, default)` returns the value at the given index or (if
#' not found) the given default value.
#' @examples
#'
#' co = container(a = 1, 2, b = 3, 4)
#' peek_at2(co, 1)
#' peek_at2(co, "a")
#' peek_at2(co, "x")
#' peek_at2(co, "x", default = 0)
NULL


#' @rdname peek_at2
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

#' @rdname peek_at2
#' @export
peekleft.Deque <- function(x, default = NULL) x$peekleft(default)


#' @name DequeS3
#' @rdname DequeS3
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


#' @rdname peek_at2
#' @return For `dict.table`, returns the column named `index` if it exist
#' otherwise the given `default` value. If the default length does not match
#' the number of rows, it is recycled accordingly and a warning is given,
#' unless the default value has a length of 1, in which case recycling is
#' done silently.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek_at2(dit, "a")
#' peek_at2(dit, 1)
#' peek_at2(dit, 3)
#' peek_at2(dit, 3, default = 9)
#' peek_at2(dit, "x")
#' peek_at2(dit, "x", default = 0)
peek_at2.dict.table <- function(x, index, default = NULL)
{
    if (has_index(x, index))
        return(.subset2(x, index))

    if (length(default) && length(default) != nrow(x)) {
        if (length(default) != 1)
            warning("length of default value (", length(default), ") ",
                    "did not match number of rows (", nrow(x), ") ",
                    "and therefore was recycled", call. = FALSE)

        default = rep_len(default, length.out = nrow(x))
    }
    default
}


#' @name dict.table
#' @rdname dict.table
#' @details
#' * `peek_at2(x, index, default = NULL)` return column named `index` if it exist
#' otherwise the given `default` value. If the default length does not match
#' the number of rows, it is recycled accordingly and a warning is given,
#' unless the default value has a length of 1, in which case recycling is
#' done silently.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek_at2(dit, "a")
#' peek_at2(dit, 1)
#' peek_at2(dit, 3)
#' peek_at2(dit, 3, default = 9)
#' peek_at2(dit, "x")
#' peek_at2(dit, "x", default = 0)
NULL


