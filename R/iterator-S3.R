#' Iterator S3 methods
#'
#' @description An `Iterator` is an object that allows to iterate over
#'  sequences. It implements `.next` and `get` to iterate and retrieve the
#'  value of the sequence it is associated with.
#' @name iterS3
#'
#' @section S3 methods for class `Iterator`:
#' \describe{
#'  \item{\code{itbegin(it)}}{Reset iterator position to 1.}
#'  \item{\code{itget(it)}}{Get value at current iterator position.}
#'  \item{\code{itget_next()}}{Get value after incrementing by one.}
#'  \item{\code{itpos()}}{Return current iterator position.}
#'  \item{\code{ithas_next(it)}}{Return TRUE if there is a next element.}
#'  \item{\code{itnext(it)}}{Increment iterator to point at next element.}
#' }
#'
#' @param it `Iterator` object
#' @examples
#' it <- iter(list("A", 1, 2))
#' while(ithas_next(it)) {
#'   print(itget_next(it))
#' }
#' ithas_next(it)   # FALSE
#' print(it)       # <Iterator> at position 3
#' itbegin(it)
#' print(it)       # <Iterator> at position 0


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

