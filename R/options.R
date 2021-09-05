.env = new.env()

.default_options = function()
{
    list(".copy"   = TRUE,
         "compare" = "all.equal",
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
#' @param .reset `logical` if TRUE, the options are reset to their default and
#' returned.
#' @param ... any options can be defined, using name = value.
#' @return
#' * container_options() returns a list of all set options sorted by name.
#' * container_options(name), a list of length one containing the set value,
#' or NULL if it is unset. Can also be multiple names (see examples).
#' * container_options(key = value) sets the option with name `key` to `value`
#' and returns the previous options invisibly.
#' @export
#' @examples
#' container_options()
#' old <- container_options(useDots = FALSE)
#' old
#' container_options("useDots")
#' container_options("useDots", "vec.len")
#' container_options(compare = "identical", foo = "bar")
#' container_options()
#' (container_options(.reset = TRUE))
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

