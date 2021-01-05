#' Iterator
#'
#' @description An `Iterator` is an object that allows to iterate over
#'  sequences. It implements `next_iter` and `get_value` to iterate and retrieve the
#'  value of the sequence it is associated with.
#' @details For more details on the methods see [Iterator()].
#'
#' @param x an object of class [Iterable()] or any other `R` object. In the
#' latter case, `x` will always be coerced to a base `R` list prior to creating
#' the `iterator`.
#' @param it `Iterator` object
#' @name iterS3
#' @seealso [Iterator()]
NULL

#' @rdname iterS3
#' @export
iter <- function(x) UseMethod("iter")

#' @rdname ContainerS3
#' @export
iter.Container <- function(x) x$iter()

#' @rdname iterS3
#' @export
iter.default <- function(x) Iterator$new(x)

#' @rdname iterS3
#' @export
is.iterator <- function(x) inherits(x, "Iterator")

#' @rdname iterS3
#' @export
begin <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$begin()
}

#' @rdname iterS3
#' @export
get_value <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get_value()
}

#' @rdname iterS3
#' @export
get_next <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get_next()
}

#' @rdname iterS3
#' @export
has_next <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$has_next()
}

#' @rdname iterS3
#' @export
has_value <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$has_value()
}

#' @rdname iterS3
#' @export
pos <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$pos()
}

#' @rdname iterS3
#' @export
next_iter <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$next_iter()
}

#' @rdname iterS3
#' @export
reset_iter <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$reset_iter()
}

#' @rdname iterS3
#' @return `length()` returns the number of elements to iterate
#' @export
length.Iterator <- function(x) x$length()

