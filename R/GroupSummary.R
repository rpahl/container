#' @export
Summary.Container <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @export
Summary.Deque <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @export
Summary.Dict <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @export
Summary.Set <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

