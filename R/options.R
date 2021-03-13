.default_options = function() {
    list("cmp" = "identical",
         "first.few" = TRUE,
         "vec.len" = 4L)
}

#' @export
container_options = local({

    options = .default_options()

    function(..., .reset = FALSE) {
        if (.reset)
            return(options <<- .default_options())

        args = list(...)
        if (!length(args))
            return(options)

        arg.names = names(args)
        if (is.null(arg.names))
            return(unlist(options[as.character(args)]))

        old = options
        new = replace(options, arg.names, args)
        new = Filter(x = new, Negate(is.null))
        options <<- new

        invisible(old)
    }
})

