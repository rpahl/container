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
#' @description An \code{\link[container]{Iterator}} is an object that
#' implements \code{get_next}, which is expected to return the next
#' element of the iterable object that it was created from.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso \code{\link[container]{Iterable}}, 
#' \code{\link[container]{Container}}, \code{\link[container]{Deque}}, 
#' \code{\link[container]{Set}}, and \code{\link[container]{Dict}}
#'
#' @section Iterator method/interface:
#' \describe{
#'  \item{\code{get_next()}}{Return next element.}
#'  \item{\code{has_next()}}{Return TRUE if there is a next element.}
#' }
#' @examples
#' # Iterator on primitive list
#' it <- Iterator$new(list("A", 1, 2))
#' while(it$has_next()) {
#'     print(it$get_next())
#' }
#' it$has_next()   # FALSE
#'
#' # Iterator from Container object
#' d <- Deque$new(1:3)
#' it <- d$iter()
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
        get_next = function() private$elems[[private$`i++`()]],
        has_next = function() private$i < length(private$elems)
    ),
    private = list(elems = vector(mode="list"), i=0,
        `i++` = function() {
            if (!self$has_next()) stop("Iterator has no more elements.")
            private$i <- private$i + 1
        }
    ),
    lock_class = TRUE
)



