#' Check for Name
#'
#' @param x any `R` object.
#' @param name `character` the name to be found.
#' @seealso [has()]
#' @export
has_name <- function(x, name) UseMethod("has_name")


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


#' @name ContainerS3
#' @rdname ContainerS3
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
        stop("name must be a character string, but got '",
             data.class(name), "'", call. = FALSE)

    if (length(name) != 1)
        stop("name must be of length 1", call. = FALSE)

    if (is.na(name))
        stop("undefined name", call. = FALSE)

    if (isTRUE(nchar(name) == 0))
        stop("name must consist of at least one character", call. = F)

    isTRUE(utils::hasName(x, name))
}


#' @name dict.table
#' @rdname dict.table
#' @details
#' * `has_name(x, name)` check if `x` has the given column name.
#' @examples
#'
#' dit = dict.table(a = 1, b = 2)
#' has_name(dit, "a")    # TRUE
#' has_name(dit, "x")    # FALSE
NULL

