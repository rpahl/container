.has_equal_containers = function(x, y, ...)
{
    if (!identical(data.class(x), data.class(y)))
        return(FALSE)

    if (length(x) != length(y))
        return(FALSE)

    xx = x$values()
    yy = y$values()

    if (!identical(names(xx), names(yy)))
        return(FALSE)

    for (i in seq_along(xx)) {
        target = xx[[i]]
        current = yy[[i]]
        if (!isTRUE(all.equal(target, current, ...)))
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
}
