#' @title Iterable abstract class interface
#' @description An \code{Iterable} is an object that provides an \code{iter()}
#' method, which is expected to return an \code{Iterator} object. This class
#' defines the abstract class interface such that each class inheriting this
#' class provides an \code{iter()} method and must implement a private method
#' \code{create_iter}, which must return an \code{\link[container]{Iterator}} object.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterator}} and \code{\link[container]{Container}}
#' @section Inherited methods:
#' Inherits method \code{iter} from abstract \code{\link[container]{Iterable}} class.
#'
#' @section Iterable method/interface:
#' \describe{
#'  \item{\code{iter()}}{Return \code{\link[container]{Iterator}} object.}
#' }
Iterable <- R6::R6Class("Iterable",
    public = list(
        initialize = function() stop("abstract class"),
        iter = function() {
            it <- private$create_iter()
            hasIterator <- inherits(it, "Iterator")
            if (!hasIterator) stop("Iterable did not create an Iterator")
            invisible(it)
        }
    ),
    private = list(create_iter = function() stop("abstract method")),
    lock_class=TRUE
)

#' @title Iterator
#' @description An \code{\link[container]{Iterator}} is an object that allows
#' to iterate over sequences. It implements \code{_next} and \code{get} to
#' iterate and retrieve the value of the sequence it is associated with.
#' @param x iterable object, e.g., \code{\link[base]{list}},
#'  \code{\link[base]{vector}}, \code{\link[container]{Container}}
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterable}},
#' \code{\link[container]{Container}}, \code{\link[container]{container}}
#'
#' @section Constructor:
#' \code{Iterator$new(x)}
#'
#' @section Iterator interface:
#' \describe{
#'  \item{\code{begin()}}{Reset iterator position to 1.}
#'  \item{\code{get()}}{Get value at current iterator position.}
#'  \item{\code{get_next()}}{Get value after incrementing by one.}
#'  \item{\code{pos()}}{Return current iterator position.}
#'  \item{\code{has_next()}}{Return TRUE if there is a next element.}
#'  \item{\code{next()}}{Increment iterator to point at next element.}
#' }
#' @section S3 method interface:
#' \describe{
#'  \item{\code{itbegin(it)}}{Reset iterator position to 1.}
#'  \item{\code{itget(it)}}{Get value at current iterator position.}
#'  \item{\code{itget_next()}}{Get value after incrementing by one.}
#'  \item{\code{itpos()}}{Return current iterator position.}
#'  \item{\code{ithas_next(it)}}{Return TRUE if there is a next element.}
#'  \item{\code{itnext(it)}}{Increment iterator to point at next element.}
#' }
#' @examples
#' # Iterator on primitive list
#' it <- Iterator$new(list("A", 1, 2))
#' while(it$has_next()) {
#' print(it$get_next())
#' }
#' it$has_next()   # FALSE
#' print(it)       # <Iterator> at position 3
#' it$begin()
#' print(it)       # <Iterator> at position 0
#' 
#' # Iterator from Container object
#' d <- deque(1:3)
#' it <- iter(d)
#' sum <- 0
#' while(it$has_next()) {
#' sum <- sum + it$get_next()
#' }
#' print(sum)
#' @export
Iterator <- R6::R6Class("Iterator",
    public = list(
        initialize = function(x=list()) {
            private$elems <- as.vector(x)
            names(private$elems) <- names(x)
            stopifnot(is.vector(private$elems))
            invisible(self)
        },
        begin = function() { 
            private$i <- 0
            invisible(self)
        },
        get = function() private$elems[[private$i]],
        get_next = function() private$`i++`()$get(),
        pos = function() private$i,
        has_next = function() private$i < length(private$elems),
        `.next` = function() private$`i++`(),
        print = function() cat("<Iterator> at position", self$pos(), "\n")
    ),
    private = list(elems = vector(mode="list"), i=0,
        `i++` = function() {
            if (self$has_next()) {
                private$i <- private$i + 1
            } else {
                stop("Iterator has no more elements.")
            }
            invisible(self)
        }
    ),
    lock_class = TRUE
)


#' @rdname Iterator
#' @param it \code{\link[container]{Iterator}} object
#' @export iter is.iterator
#' @examples
#' 
#' # S3 method interface
#' it <- iter(list("A", 1, 2))
#' while(ithas_next(it)) {
#' print(itget_next(it))
#' }
#' ithas_next(it)   # FALSE
#' print(it)       # <Iterator> at position 3
#' itbegin(it)
#' print(it)       # <Iterator> at position 0
iter <- function(x) UseMethod("iter")

#' @export
iter.default <- function(x) Iterator$new(x)

#' @rdname Iterator
is.iterator <- function(x) inherits(x, "Iterator")

#' @rdname Iterator
#' @export
itbegin <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$begin()
}

#' @rdname Iterator
#' @export
itget <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get()
}

#' @rdname Iterator
#' @export
itget_next <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get_next()
}

#' @rdname Iterator
#' @export
itpos <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$pos()
}

#' @rdname Iterator
#' @export
ithas_next <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$has_next()
}

#' @rdname Iterator
#' @export
itnext <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$.next()
}

