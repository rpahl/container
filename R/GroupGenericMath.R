#' @name ContainerS3
#' @rdname ContainerS3
#' @examples
#' # Math
#' co = container(1, 2, -(3:5))
#' co
#' abs(co)
#' cumsum(co)
#' round(co)
#' exp(co)
#'
NULL

#' @export
Math.Container <- function(x, ...)
{
    as.container(get(.Generic)(unpack(x), ...))
}

#' @name DequeS3
#' @rdname DequeS3
#' @examples
#' # Math
#' d = deque(1, 2, -(3:5))
#' d
#' abs(d)
#' cumsum(d)
#' round(d)
#' exp(d)
#'
NULL

#' @export
Math.Deque <- function(x, ...)
{
    as.deque(get(.Generic)(unpack(x), ...))
}

#' @name DictS3
#' @rdname DictS3
#' @examples
#' # Math
#' d = dict(a = rnorm(1), b = rnorm(1))
#' abs(d)
#' cumsum(d)
#' round(d)
#' exp(d)
#'
NULL

#' @export
Math.Dict <- function(x, ...)
{
    as.dict(get(.Generic)(unpack(x), ...))
}

#' @name SetS3
#' @rdname SetS3
#' @examples
#' # Math
#' s = setnew(5:3, 1, 2)
#' s
#' abs(s)
#' cumsum(s)
#' round(s)
#' exp(s)
#'
NULL

#' @export
Math.Set <- function(x, ...)
{
    as.set(get(.Generic)(unpack(x), ...))
}

