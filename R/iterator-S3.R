#' Iterator S3 methods
#'
#' @description An `Iterator` is an object that allows to iterate over
#'  sequences. It implements `.next` and `get` to iterate and retrieve the
#'  value of the sequence it is associated with.
#' @name iterS3
#'
#' @section S3 methods for class `Iterator`:
#'  * `itbegin(it)` reset iterator position to 1.
#'  * `itget(it)` get value at current iterator position.
#'  * `itget_next()` get value after incrementing by one.
#'  * `itpos()` return current iterator position.
#'  * `ithas_next(it)` return `TRUE` if there is a next element.
#'  * `itnext(it)` increment iterator to point at next element.
#'
#' @param it `Iterator` object
NULL

#' @rdname iterS3
#' @export
iter <- function(x) UseMethod("iter")

#' @rdname iterS3
#' @export
iter.default <- function(x) Iterator$new(x)

#' @rdname iterS3
#' @export
is.iterator <- function(x) inherits(x, "Iterator")

#' @rdname iterS3
#' @export
itbegin <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$begin()
}

#' @rdname iterS3
#' @export
itget <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get()
}

#' @rdname iterS3
#' @export
itget_next <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get_next()
}

#' @rdname iterS3
#' @export
itpos <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$pos()
}

#' @rdname iterS3
#' @export
ithas_next <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$has_next()
}

#' @rdname iterS3
#' @export
itnext <- function(it)
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$.next()
}

