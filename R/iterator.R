#' @title Iterable abstract class interface
#' @description An \code{Iterable} is an object that provides an \code{iterator()}
#' method, which is expected to return an \code{Iterator} object. This class
#' defines the abstract class interface such that each class inheriting this
#' class provides an \code{iterator()} method and must implement a private method
#' \code{create_iter}, which must return an \code{\link[container]{Iterator}} object.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterator}} and \code{\link[container]{Container}}
#' @section Inherited methods:
#' Inherits method \code{iterator} from abstract \code{\link[container]{Iterable}} class.
#'
#' @section Iterable method/interface:
#' \describe{
#'  \item{\code{iterator()}}{Return \code{\link[container]{Iterator}} object.}
#' }
Iterable <- R6::R6Class("Iterable",
    public = list(
        initialize = function() stop("abstract class"),
        iterator = function() {
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
#' @usage Iterator$new(x)
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterable}},
#' \code{\link[container]{Container}}, \code{\link[container]{container}}
#'
#' @section Iterator interface:
#' \describe{
#'  \item{\code{begin()}}{Reset iterator position to 1.}
#'  \item{\code{get()}}{Get value at current iterator position.}
#'  \item{\code{get.next()}}{Get value after incrementing by one.}
#'  \item{\code{pos()}}{Return current iterator position.}
#'  \item{\code{has.next()}}{Return TRUE if there is a next element.}
#'  \item{\code{next()}}{Increment iterator to point at next element.}
#' }
#' @section S3 method interface:
#' \describe{
#'  \item{\code{itbegin(it)}}{Reset iterator position to 1.}
#'  \item{\code{itget(it)}}{Get value at current iterator position.}
#'  \item{\code{itget.next()}}{Get value after incrementing by one.}
#'  \item{\code{itpos()}}{Return current iterator position.}
#'  \item{\code{ithas.next(it)}}{Return TRUE if there is a next element.}
#'  \item{\code{itnext(it)}}{Increment iterator to point at next element.}
#' }
#' @examples
#' # Iterator on primitive list
#' it <- Iterator$new(list("A", 1, 2))
#' while(it$has.next()) {
#' print(it$get.next())
#' }
#' it$has.next()   # FALSE
#' print(it)       # <Iterator> at position 3
#' it$begin()
#' print(it)       # <Iterator> at position 0
#' 
#' # Iterator from Container object
#' d <- deque(1:3)
#' it <- iterator(d)
#' sum <- 0
#' while(it$has.next()) {
#' sum <- sum + it$get.next()
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
        get.next = function() private$`i++`()$get(),
        pos = function() private$i,
        has.next = function() private$i < length(private$elems),
        `.next` = function() private$`i++`(),
        print = function() cat("<Iterator> at position", self$pos(), "\n")
    ),
    private = list(elems = vector(mode="list"), i=0,
        `i++` = function() {
            if (self$has.next()) {
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
#' @export iterator is.iterator itget itpos ithas.next itnext
#' @examples
#' 
#' # S3 method interface
#' it <- iterator(list("A", 1, 2))
#' while(ithas.next(it)) {
#' print(itget.next(it))
#' }
#' ithas.next(it)   # FALSE
#' print(it)       # <Iterator> at position 3
#' itbegin(it)
#' print(it)       # <Iterator> at position 0
#' @rdname Iterator
iterator <- function(x) UseMethod("iterator")

#' @rdname Iterator
iterator.default <- function(x) Iterator$new(x)

#' @rdname Iterator
iterator.Container <- function(cont) cont$iterator()

#' @rdname Iterator
is.iterator <- function(x) inherits(x, "Iterator")

#' @rdname Iterator
itbegin <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$begin()
}

#' @rdname Iterator
itget <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get()
}

#' @rdname Iterator
itget.next <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$get.next()
}

#' @rdname Iterator
itpos <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$pos()
}

#' @rdname Iterator
ithas.next <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$has.next()
}

#' @rdname Iterator
itnext <- function(it) 
{
    if (!is.iterator(it)) stop("arg must be an Iterator")
    it$.next()
}

