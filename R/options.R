.env = new.env()

.default_options = function()
{
    list("compare" = "all.equal",
         "useDots" = TRUE,
         "vec.len" = 4L)
}

.env[["options"]] = .default_options()


#' Set container package options
#'
#' @param ... any options can be defined, using name = value.
#' @param x a character string holding an option name.
#' @param default if the specified option is not set in the options list, this
#' value is returned.
#' @param .reset `logical` if `TRUE`, the options are reset to their default and
#' returned.
#' @param ... any options can be defined, using name = value.
#' @return
#' * `container_options()` returns a list of all set options sorted by name.
#' * `container_options(name)`, a list of length one containing the set value,
#' or `NULL` if it is unset. Can also be multiple names (see Examples).
#' * `container_options(key = value)` sets the option with name `key` to `value`
#' and returns the previous options invisibly.
#'
#' @section Container Options:
#'  * `compare` (default = `all.equal`)
#'  * `useDots` (default = `TRUE`) whether to abbreviate long container
#'  elements with `...` when exceeding `vec.len` (see below). If `FALSE`, they
#'  are abbreviated as `<<type(length)>>`.
#'  * `vec.len` (default = 4) the length limit at which container vectors are
#'  abbreviated.
#'
#' @export
#' @examples
#' co = container(1L, 1:10, as.list(1:5))
#' co
#'
#' container_options(useDots = FALSE)
#' co
#'
#' container_options(useDots = TRUE, vec.len = 6)
#' co
#'
#' has(co, 1.0)
#'
#' container_options(compare = "identical")
#'
#' has(co, 1.0) # still uses 'all.equal'
#'
#' co2 = container(1L)
#' has(co2, 1.0)
#' has(co2, 1L)
#'
#' container_options()
#' container_options(.reset = TRUE)
container_options <-
function(..., .reset = FALSE)
{
    if (.reset) {
        .env[["options"]] = .default_options()
        return(.env[["options"]])
    }

    args = list(...)
    if (!length(args))
        return(.env[["options"]])

    arg.names = names(args)
    if (is.null(arg.names))
        return(Filter(x = .env[["options"]][as.character(args)],
                      f = Negate(is.null)))

    old = .env[["options"]]
    new = replace(.env[["options"]], arg.names, args)
    new = Filter(x = new, f = Negate(is.null))
    new = new[sort(names(new))]
    .env[["options"]] = new

    invisible(old)
}


#' @rdname container_options
#' @export
getContainerOption = function(x, default = NULL)
{
    if (!is_string(x))
        stop("'x' must be a character string")


    if (utils::hasName(container_options(), x))
        container_options()[[x]]
    else
        default
}

