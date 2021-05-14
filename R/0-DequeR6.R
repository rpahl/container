#' Deque (double-ended queue)
#'
#' @description Deques are a generalization of stacks and queues typically
#' with methods to add, delete and access elements at both sides of the
#' underlying data sequence. As such, the [Deque()] can also be used to mimic
#' both stacks and queues.
#' @details This class inherits from class [Container()] and extends it by
#' `pop` and `peek` methods, element counting, and reverse and rotate
#' functionality.
#' @importFrom R6 R6Class
#' @seealso [Container()], [deque()]
#' @export
Deque <- R6::R6Class("Deque",
    inherit = Container,
    public = list(
        #' @description Add element to left side of the `Deque`.
        #' @param ... elements to be added.
        #' @return the `Deque()` object.
        addleft = function(...) {
            elems = list(...)

            if (length(elems) == 0)
                return(self)

            if (length(elems) > 1) {
                for (i in seq_along(elems))
                    do.call(self$addleft, elems[i])

                return(self)
            }

            private$elems <- c(elems, private$elems)
            self
        },

        #' @description Peek at last element on the right without removing it.
        #' @param default returned default value if `Deque` is empty.
        #' @return element 'peeked' on the right
        peek = function(default = NULL) {
            if (self$is_empty())
                return(default)

            .subset2(private$elems, self$length())
        },

        #' @description Peek at first element on the left without removing it.
        #' @param default returned default value if `Deque` is empty.
        #' @return element 'peeked' on the left
        peekleft = function(default = NULL) {
            if (self$is_empty())
                return(default)

            .subset2(private$elems, 1)
        },

        #' @description
        #' Delete and return element from the right side of the [Deque()].
        #' @return element 'popped' from the right side of the [Deque()]
        pop = function() {
            if (self$is_empty())
                stop("pop at empty ", data.class(self))

            last <- self$peek()
            self$delete(last)
            last
        },

        #' @description
        #' Delete and return element from the left side of the [Deque()].
        #' @return element 'popped' from the left side of the [Deque()]
        popleft = function() {
            if (self$is_empty())
                stop("popleft at empty ", data.class(self))

            first <- self$peekleft()
            self$delete(first)
            first
        },

        #' @description Reverse all elements of the [Deque()] in-place.
        #' @return the `Deque()` object.
        rev = function() {
            len <- self$length()
            private$elems <- rev(private$elems)
            self
        },

        #' @description Rotate all elements `n` steps to the right. If n is
        #' negative, rotate to the left.
        #' @param n `integer` number of steps to rotate
        #' @return returns the `Deque()` object.
        rotate = function(n = 1L) {
            if (self$is_empty() || n == 0)
                return(self)

            if (n > 0L) {
                last <- self$pop()
                self$addleft(last)
                self$rotate(n - 1)
            } else {
                first <- self$popleft()
                self$add(first)
                self$rotate(n + 1)
            }
        }
    ),
    lock_class = TRUE
)

