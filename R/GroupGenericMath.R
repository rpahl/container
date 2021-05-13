#' @name OpsMath
#' @rdname ContainerS3
#' @examples
#'
#' # Math
#' set.seed(123)
#' co = as.container(rnorm(3))
#' abs(co)
#' sum(co)
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

#' @name OpsMath
#' @rdname DequeS3
#' @examples
#'
#' # Math
#' set.seed(123)
#' d = as.deque(rnorm(3))
#' abs(d)
#' sum(d)
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

#' @name OpsMath
#' @rdname DictS3
#' @examples
#'
#' # Math
#' set.seed(123)
#' d = dict(a = rnorm(1), b = rnorm(1))
#' abs(d)
#' sum(d)
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

#' @name OpsMath
#' @rdname SetS3
#' @examples
#'
#' # Math
#' set.seed(123)
#' s = as.set(rnorm(3))
#' abs(s)
#' sum(s)
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

