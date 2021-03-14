.default_options = function()
{
    list("compare" = "all.equal",
         "useDots" = TRUE,
         "vec.len" = 4L)
}

options = .default_options()


#' Set container package options
#'
#' @param ... any options can be defined, using name = value.
#' @param .reset `logical` if TRUE, the options are reset to their default and
#' returned.
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
    if (.reset)
        return(options <<- .default_options())

    args = list(...)
    if (!length(args))
        return(options)

    arg.names = names(args)
    if (is.null(arg.names))
        return(Filter(x = options[as.character(args)], f = Negate(is.null)))

    old = options
    new = replace(options, arg.names, args)
    new = Filter(x = new, f = Negate(is.null))
    new = new[sort(names(new))]
    options <<- new

    invisible(old)
}

