#' @export
Math.Container <- function(x, ...)
{
    as.container(get(.Generic)(unpack(x), ...))
}

#' @export
Math.Deque <- function(x, ...)
{
    as.deque(get(.Generic)(unpack(x), ...))
}

#' @export
Math.Dict <- function(x, ...)
{
    as.dict(get(.Generic)(unpack(x), ...))
}

#' @export
Math.Set <- function(x, ...)
{
    as.set(get(.Generic)(unpack(x), ...))
}

