#' Check if object is subsettable
#'
#' @param x any `R` object
#' @param .subset subset function to be used on `x`
#' @return returns `TRUE` if object is subsettable otherwise `FALSE`
#' @export
is.subsettable <- function(x, .subset = .subset2)
{
    if (!is.function(.subset))
        stop("'.subset' argument must be a function", call. = FALSE)

    res = tryCatch(.subset(x, 1), error = identity)
    !inherits(res, "error")
}


#' @title Iterator
#'
#' @description An `Iterator` is an object that allows to iterate over
#'  sequences. It implements `next_iter` and `get_value` to iterate and retrieve the
#'  value of the sequence it is associated with.
#' @author Roman Pahl
#' @docType class
#' @importFrom R6 R6Class
#' @export
Iterator <- R6::R6Class("Iterator",
    public = list(

        #' @description `Iterator` constructor
        #' @param x object to iterate over
        #' @param .subset accessor function
        #' @return invisibly returns the `Iterator` object
        initialize = function(x, .subset = .subset2) {
            if (!(is.iterable(x) || is.subsettable(x, .subset)))
                stop("x must be iterable or subsettable", call. = FALSE)

            private$object <- x
            private$.subset <- .subset
            invisible(self)
        },

        #' @description set iterator to the first element of the underlying
        #' sequence unless length of sequence is zero, in which case it will
        #' point to nothing.
        #' @return invisibly returns the `Iterator` object
        begin = function() {
            private$i <- min(1L, self$length())
            invisible(self)
        },

        #' @description get value where the iterator points to
        #' @return returns the value the `Iterator` is pointing at.
        get_value = function() {
            if (!self$has_value()) {
                stop("iterator does not point at a value")
            }
            private$.subset(private$object, self$pos())
        },

        #' @description get next value
        #' @return increments the iterator and returns the value the `Iterator`
        #' is pointing to.
        get_next = function() {
            self$next_iter()$get_value()
        },

        #' @description check if `iterator` has more elements
        #' @return `TRUE` if `iterator` has next element else `FALSE`
        has_next = function() {
            private$i < self$length()
        },

        #' @description check if `iterator` points at value
        #' @return `TRUE` if `iterator` points at value otherwise `FALSE`
        has_value = function() {
            self$pos() > 0 && self$pos() <= self$length()
        },

        #' @description iterator length
        #' @return number of elements to iterate
        length = function() {
            length(private$object)
        },

        #' @description get iterator position
        #' @return `integer` if `iterator` has next element else `FALSE`
        pos = function() {
            private$i
        },

        #' @description increment `iterator`
        #' @return invisibly returns the `Iterator` object
        next_iter = function() {
            if (self$has_next()) {
                private$i <- private$i + 1
            } else {
                stop("Iterator has no more elements.")
            }
            invisible(self)
        },

        #' @description print method
        print = function() {
            cat("<Iterator> at position",
                self$pos(), "/", self$length(), "\n")
        },

        #' @description reset iterator to '0'
        #' @return invisibly returns the `Iterator` object
        reset_iter = function() {
            private$i <- 0L
            invisible(self)
        }
    ),
    private = list(object = list(),
                   .subset = NULL,
                   i = 0L),
    lock_class = TRUE
)

