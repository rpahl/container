#' Check if object has some name
#'
#' @param x any `R` object.
#' @param name `character` the name to be found.
#' @param ... additional arguments to be passed to or from methods.
#' @seealso [has()]
#' @export
has_name <- function(x, ...) UseMethod("has_name")


#' @rdname has_name
#' @return `TRUE` if name is in `x` and otherwise `FALSE`.
#' @export
#' @examples
#'
#' co = container(a = 1, 2, f = mean)
#' has_name(co, "a")    # TRUE
#' has_name(co, "f")    # TRUE
#' has_name(co, "2")    # FALSE
has_name.Container <- function(x, name)
{
    if (missing(name))
        return(isTRUE(length(names(x)) > 0))

    x$has_name(name)
}


#' @name has_name.Container
#' @rdname ContainerS3
#' @param name `character` the name to be found.
#' @usage
#' has_name(x, name)
#' @details
#' * `has_name(x, name)` check if `name` is in `x`
#' @examples
#'
#' co = container(a = 1, 2, f = mean)
#' has_name(co, "a")    # TRUE
#' has_name(co, "f")    # TRUE
#' has_name(co, "2")    # FALSE
NULL


#' @rdname has_name
#' @return For `dict.table` `TRUE` if the dict.table objects has the given
#' column name, otherwise `FALSE`.
#' @export
#' @examples
#'
#' dit = dict.table(a = 1:2, b = 3:4)
#' has_name(dit, "a")   # TRUE
#' has_name(dit, "x")   # FALSE
has_name.dict.table <- function(x, name)
{
    if (missing(name))
        return(isTRUE(length(names(x)) > 0))

    if (!is.character(name))
        stop("expected a character string, but got '",
             data.class(name), "'", call. = FALSE)

    if (length(name) != 1)
        stop("name must be of length 1", call. = FALSE)

    if (is.na(name))
        stop("undefined name", call. = FALSE)

    if (isTRUE(nchar(name) == 0))
        stop("name must consist of at least one character", call. = F)

    isTRUE(utils::hasName(x, name))
}


#' @name has.dict.table
#' @rdname dict.table
#' @param name `character` the name to be found.
#' @usage
#' has_name(x, name)
#' @details
#' * `has_name(x, name)` check if `x` has the given column name.
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' has_name(dit, "a")    # TRUE
#' has_name(dit, 1)      # TRUE
#' has_name(dit, "x")    # FALSE
#' has_name(dit, 3)      # FALSE
NULL

