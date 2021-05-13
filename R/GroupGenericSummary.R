#' @name OpsSummary
#' @rdname ContainerS3
#' @examples
#' # Summary
#' range(co)
#' min(co)
#' max(co)
#'
NULL

#' @export
Summary.Container <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @name OpsSummary
#' @rdname DequeS3
#' @examples
#' # Summary
#' range(d)
#' min(d)
#' max(d)
#'
NULL

#' @export
Summary.Deque <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @name OpsSummary
#' @rdname DictS3
#' @examples
#' # Summary
#' range(d)
#' min(d)
#' max(d)
#'
NULL

#' @export
Summary.Dict <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

#' @name OpsSummary
#' @rdname SetS3
#' @examples
#' # Summary
#' range(s)
#' min(s)
#' max(s)
#'
NULL

#' @export
Summary.Set <- function(..., na.rm = FALSE)
{
    get(.Generic)(unlist(lapply(..., unpack)), na.rm = na.rm)
}

