#' Iterate over Sequences
#'
#' @description An `Iterator` is an object that allows to iterate over
#' sequences. It implements [next_iter()] and [get_value()] to iterate and
#' retrieve the value of the sequence it is associated with.
#' For documentation of the methods see [Iterator].
#'
#' @param x an object of class [Iterable] or any other `R` object. In the
#' latter case, `x` will always be coerced to a base `R` [list] prior to
#' creating the [Iterator].
#' @param ... other parameters passed to or from methods
#' @param it `Iterator` object
#' @name iterS3
#' @seealso For the class documentation see [Iterator].
#' @examples
#' # Numeric Vector
#' v = 1:3
#' it = iter(v)
#' it
#'
#' try(it$get_value())  # iterator does not point at a value
#'
#' has_value(it)
#' has_next(it)
#' next_iter(it)
#' get_value(it)
#' get_next(it)
#' get_next(it)
#' it
#' has_next(it)
#' begin(it)
#' get_value(it)
#' reset_iter(it)
#'
#' # Works on copy of Container
#' co = container(1, 2, 3)
#' it = iter(co)
#' get_next(it)
#' ref_discard(co, 2)
#' co
#' it
#' get_next(it)
#' ref_clear(co)
#' co
#' it
#' get_next(it)
#' begin(it)
#' @rdname iterS3
#' @export
iter <- function(x, ...) UseMethod("iter")

#' @rdname iterS3
#' @export
iter.Container <- function(x, ...) x$clone(deep = TRUE)$iter()

#' @rdname iterS3
#' @export
iter.default <- function(x, ...) Iterator$new(x, ...)

#' @rdname iterS3
#' @export
is.iterator <- function(x) inherits(x, "Iterator")

#' @rdname iterS3
#' @export
is.iterable <- function(x)
{
    inherits(x, "Iterable")
}


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
#' @return `length` returns the number of elements that can be iterated over.
#' @export
length.Iterator <- function(x) x$length()

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
