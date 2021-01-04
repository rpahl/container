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
        #' @param x object to iterate over
        #' @return invisibly returns the `Iterator` object
        initialize = function(x) {
            if (is.iterable(x)) {
                return(x$iter())
            } else {
                elems <- as.list(x)
                names(elems) <- seq_along(elems)
            }
            private$.clear()
            self$reset_iter()
            private$env <- list2env(elems, envir = private$env)
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
            name <- as.character(private$i)
            tryCatch(get(name, private$env, inherits = FALSE),
                     error = function(e) stop("iterator does not point at a value"))
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

        #' @description iterator length
        #' @return number of elements to iterate
        length = function() {
            length(private$env)
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
    private = list(env = new.env(),
                   i = 0L,
                   .clear = function() {
                       remove(list = ls(private$env), envir = private$env)
                   }
    ),
    lock_class = TRUE
)

