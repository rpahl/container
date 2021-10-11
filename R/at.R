#' Strict element(s) extraction
#'
#' Extract parts of a Container at given indices. If an index is invalid, an
#' error is signaled. If given as a string, the element matching the
#' name is returned. If there are two or more identical names, the value of the
#' first match (i.e. *leftmost* element) is returned. Indices can be letters or
#' numbers, or both at the same time.
#' @param .x an `R` object of the respective class.
#' @param ... indices of elements to be extracted
#' @seealso [peek_at()] for less strict extraction
#' @export
at <- function(.x, ...) UseMethod("at")


#' @rdname at
#' @return For `Container`, returns the values at the given indidces.
#' @export
#' @examples
#'
#' # Container
#' co = container(a = 1, 2, b = 3, 4)
#' at(co, 1:3)
#' at(co, "a", "b", 2)
#' \dontrun{
#' at(co, "x")     # index 'x' not found
#' at(co, 1:10)    # index 5 exceeds length of Container
#' }
#' # Dict
#' d = dict(a = 1, b = 3)
#' at(d, 1:2)
#' at(d, "a", 2)
#' \dontrun{
#' at(d, "x")     # index 'x' not found
#' at(d, 1:3)     # index 5 exceeds length of Dict
#' }
at.Container <- function(.x, ...)
{
    indices = list(...)
    if (!length(indices))
        return(.x)

    l = lapply(indices, function(index) .x$at(index))
    Reduce(l, f = c)
}


#' @name ContainerS3
#' @rdname ContainerS3
#' @details
#' * `at(.x, ...,)` returns the value at the given indices. Indices
#' can be letters or numbers or both. All indices must exist.
#' @examples
#'
#' co = container(a = 1, 2, b = 3, 4)
#' at(co, 1:3)
#' at(co, "a", "b", 2)
#' \dontrun{
#' at(co, "x")     # index 'x' not found
#' at(co, 1:10)    # index 5 exceeds length of Container
#' }
NULL


#' @rdname at
#' @return For `dict.table`, returns the columns at the given indices.
#' @export
#' @examples
#'
#' # dict.table
#' dit = dict.table(a = 1:3, b = 4:6)
#' at(dit, "a")
#' at(dit, 2)
#' at(dit, "a", 2)
#' \dontrun{
#' at(dit, "x")     # index 'x' not found
#' at(dit, 1:3)     # index 3 exceeds length of dict.table
#' }
at.dict.table <- function(.x, ...)
{
    args = list(...)
    if (!length(args))
        return(.x)

    if (length(args) == 1 && is.null(args[[1]]))
        return(dict.table())

    lapply(args, function(index) lapply(index, .assert_index_and_arg, x = .x))

    # Apply first on dict object to determine all the column names
    d = at(as.dict(.x), ...)

    l = lapply(names(d), at2.dict.table, x = .x)
    names(l) = names(d)
    as.dict.table(l)
}


#' @name dicttable
#' @rdname dicttable
#' @details
#' * `at(.x, ...)` returns the columns at the given indices. Indices
#' can be letters or numbers or both. All columns must exist.
#' @examples
#'
#' dit = dict.table(a = 1:3, b = 4:6)
#' at(dit, "a")
#' at(dit, 2)
#' at(dit, "a", 2)
#' \dontrun{
#' at(dit, "x")     # index 'x' not found
#' at(dit, 1:3)     # index 3 exceeds length of dict.table
#' }
NULL


