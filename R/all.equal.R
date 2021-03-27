.has_equal_containers = function(x, y, ...)
{
    if (!identical(data.class(x), data.class(y)))
        return(FALSE)

    if (length(x) != length(y))
        return(FALSE)


    if (!identical(names(x), names(y)))
        return(FALSE)

    it.x = x$iter()
    it.y = y$iter()

    is_equal = match.fun(container_options("compare")[[1]])

    while (it.x$has_next()) {
        target = it.x$get_next()[[1]]
        current = it.y$get_next()[[1]]
        if (!isTRUE(is_equal(target, current, ...)))
            return(FALSE)
    }

    TRUE
}


#' @export
all.equal.Container = function(target, current, ...)
{
    if (.has_equal_containers(target, current))
        return(TRUE)

    # TODO: construct messages for non-equality case
    FALSE
}

