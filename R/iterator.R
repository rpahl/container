#' Iterable abstract class interface
#'
#' @description An `Iterable` is an object that provides an [iter()] method,
#' which is expected to return an `Iterator` object. This class defines the
#' abstract class interface such that each class inheriting this class provides
#' an [iter()] method and must implement a private method [create_iter()],
#' which must return an `Iterator` object.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @seealso `Iterator` and `Container`
Iterable <- R6::R6Class("Iterable",
    public = list(

        #' @description
        #' `Iterable` is an abstract class and thus cannot be instantiated.
        initialize = function() stop("abstract class"),

        #' @description Create iterator
        #' @return invisibly returns the `Iterator` object.
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
#'
#' @description An `Iterator` is an object that allows to iterate over
#'  sequences. It implements `.next` and `get` to iterate and retrieve the
#'  value of the sequence it is associated with.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#'
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

        #' @description `Iterator` constructor
        #' @param x sequence to iterate over
        #' @return invisibly returns `Iterator` object
        initialize = function(x = list()) {
            private$elems <- as.vector(x)
            names(private$elems) <- names(x)
            stopifnot(is.vector(private$elems))
            invisible(self)
        },

        #' @description set iterator to start of sequence
        begin = function() {
            private$i <- 0
            invisible(self)
        },

        #' @description get element where the iterator points to
        get = function() {
            private$elems[[private$i]]
        },

        #' @description get next element
        get_next = function() {
            private$`i++`()$get()
        },

        #' @description get iterator position
        pos = function() {
            private$i
        },

        #' @description check if `iterator` has more elements
        #' @return `TRUE` if `iterator` has next element else `FALSE`
        has_next = function() {
            private$i < length(private$elems)
        },

        #' @description increment `iterator`
        `.next` = function() {
            private$`i++`()
        },

        #' @description print method
        print = function() {
            cat("<Iterator> at position", self$pos(), "\n")
        }
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

