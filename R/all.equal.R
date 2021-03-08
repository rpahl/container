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

#' TODO: construct messages if non-equality as is typical for all.equal
#' @export
all.equal.Container = function(target, current, ...)
{
    .has_equal_containers(target, current)

}

