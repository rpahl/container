#' @export
Math.Container <- function(x, ...)
{
    as.container(get(.Generic)(unlist(x$values()), ...))
}

#' @export
Math.Deque <- function(x, ...)
{
    as.deque(get(.Generic)(unlist(x$values()), ...))
}

#' @export
Math.Dict <- function(x, ...)
{
    as.dict(get(.Generic)(unlist(x$values()), ...))
}

#' @export
Math.Set <- function(x, ...)
{
    as.set(get(.Generic)(x$values(), ...))
}

