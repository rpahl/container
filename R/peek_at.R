#' Peek at element(s)
#'
#' Try to access elements and return default values if not found.
#' In contrast to `[at()]`, this function provides a less stricter element
#' access, that is, it remains valid even if elements don't exist.
#' @details
#' `peek_at` tries to access specific values.
#'
#' @param .x an `R` object of the respective class.
#' @param ... indices of elements to be extracted
#' @param .default value to be returned if peeked value does not exist.
#' @seealso [at()] for strict element extraction
#' @export
peek_at <- function(.x, ...) UseMethod("peek_at")


#' @rdname peek_at
#' @return For `Container`, returns the value at the given indices or (if not
#' found) the given default value.
#' @export
#' @examples
#'
#' # Container
#' co = container(a = 1, 2, b = 3, 4)
#' peek_at(co, 1)
#' peek_at(co, "a")
#' peek_at(co, "x")
#' peek_at(co, "x", .default = 0)
#' peek_at(co, "a", "x", 2, 9, .default = -1)
#'
#' # Dict
#' d = dict(a = 1, b = 1:3)
#' peek_at(d, "b")
#' peek_at(d, "x")
#' peek_at(d, "x", .default = 4:7)
peek_at.Container <- function(.x, ..., .default = NULL)
{
    indices = list(...)
    if (!length(indices))
        return(.x)

    peek_at_index = function(index)
        .x$peek_at(index, default = .default)

    l = lapply(indices, peek_at_index)
    Reduce(l, f = c)
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `peek_at(x, ..., .default = NULL)` returns the value at the given indices
#' or (if not found) the given default value.
#' @examples
#'
#' co = container(a = 1, 2, b = 3, 4)
#' peek_at(co, 1)
#' peek_at(co, "a")
#' peek_at(co, "x")
#' peek_at(co, "x", .default = 0)
#' peek_at(co, "a", "x", 2, 9, .default = -1)
NULL



#' @rdname peek_at
#' @return For `dict.table`, returns the columns at the given indices or (if not
#' found) columns with the given default value.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek_at(dit, "a")
#' peek_at(dit, 1)
#' peek_at(dit, 3)
#' peek_at(dit, "x")
#' peek_at(dit, "x", .default = 0)
#' peek_at(dit, "a", "x", .default = 0)
peek_at.dict.table <- function(.x, ..., .default = NULL)
{
    # Apply first on dict object to determine all the column names
    d = peek_at(as.dict(.x), ..., .default = .default)

    l = lapply(names(d), peek_at2.dict.table, x = .x, default = .default)
    names(l) = names(d)
    as.dict.table(l)
}


#' @name dict.table
#' @rdname dict.table
#' @details
#' * `peek_at(x, ..., .default = NULL)` returns the columns at the given
#' indices or (if not found) columns with the given default value.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' peek_at(dit, "a")
#' peek_at(dit, 1)
#' peek_at(dit, 3)
#' peek_at(dit, "x")
#' peek_at(dit, "x", .default = 0)
#' peek_at(dit, "a", "x", .default = 0)
NULL

