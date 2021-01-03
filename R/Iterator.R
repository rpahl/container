#' Check if object is subsettable
#'
#' @param any `R` object
#' @return returns `TRUE` if object is subsettable otherwise `FALSE`
#' @export
is.subsettable <- function(x)
{
    if (length(x) == 0) return(FALSE)

    res = tryCatch(.subset2(x, 1), error = identity)
    !inherits(res, "error")
}


#' Check if object is iterable
#'
#' @param any `R` object
#' @return returns `TRUE` if object is iterable otherwise `FALSE`
#' @export
is.iterable <- function(x)
{
    inherits(x, "Iterable")
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
        #' @param x sequence to iterate over
        #' @return invisibly returns `Iterator` object
        initialize = function(x) {
            if (is.iterable(x)) {
                return(x$iter())
            }
            if (!is.vector(x)) {
                stop("'x' must be at least a vector")
            }
            private$elems <- x
            invisible(self)
        },

        #' @description set iterator to start of sequence
        begin = function() {
            private$i <- 0
            invisible(self)
        },

        #' @description get value where the iterator points to
        get_value = function() {
            .subset2(private$elems, private$i)
        },

        #' @description get next element
        get_next = function() {
            private$`i++`()$get_value()
        },

        #' @description check if `iterator` has more elements
        #' @return `TRUE` if `iterator` has next element else `FALSE`
        has_next = function() {
            private$i < length(private$elems)
        },

        #' @description get iterator position
        pos = function() {
            private$i
        },

        #' @description increment `iterator`
        next_iter = function() {
            private$`i++`()
        },

        #' @description print method
        print = function() {
            cat("<Iterator> at position", self$pos(), "\n")
        }
    ),
    private = list(elems = NULL, i = 0,
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

